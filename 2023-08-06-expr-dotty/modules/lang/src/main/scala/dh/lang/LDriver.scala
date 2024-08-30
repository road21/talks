package dh.lang

import cats.data.NonEmptyVector
import dotty.tools.dotc.ast.{Trees, untpd}
import dotty.tools.dotc.ast.Trees.Untyped
import dotty.tools.dotc.ast.untpd.{Block, DefDef, Ident, Import, ImportSelector, ModuleDef, PackageDef, Select, Template, TypeTree, ValDef}
import dotty.tools.dotc.{CompilationUnit, Compiler, Driver, Run, ast, report, sbt, semanticdb}
import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.{Flags, Mode, Names}
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty.TASTYRun
import dotty.tools.dotc.transform.{PostTyper, SetRootTree, YCheckPositions, sjs}
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Stats.record
import dotty.tools.io.{Directory, PlainDirectory}
import dotty.tools.io
import dotty.tools.scripting.Util.deleteFile
import dotty.tools.unsupported
import dotty.tools.dotc.typer.ImportInfo.*
import dotty.tools.dotc.util.Spans
import dh.lang.LTranslator.{calcCls, funcName}
import dh.lang.core.{Arguments, LType}
import dotty.tools.MainGenericRunner.classpathSeparator
import dotty.tools.dotc.core.Flags.SyntheticTermParam
import dotty.tools.dotc.reporting.{Message, MessageKind as DottyMsgKind}

import java.io.{BufferedOutputStream, BufferedReader, FileOutputStream, InputStreamReader, PrintWriter}
import java.net.URLClassLoader
import java.nio.file.{Files, Path}
import scala.util.{Failure, Try, Using}
import errors.{Compile, CompileAndRunError, CompileError, Internal, RunError}
import cats.syntax.vector.*
import dh.lang.data.ByteCode
import dotty.tools.dotc.reporting.ConsoleReporter

import java.lang.reflect.InvocationTargetException

class LCompiler(term: LTree, outputType: LType) extends Compiler:
  override protected def frontendPhases: List[List[Phase]] =
    List(new LTranslator(term, outputType)) ::
      List(new TyperPhase) ::                   // Compiler frontend: namer, typer
      List(new YCheckPositions) ::              // YCheck positions
      List(new sbt.ExtractDependencies) ::      // Sends information on classes' dependencies to sbt via callbacks
      List(new semanticdb.ExtractSemanticDB) :: // Extract info into .semanticdb files
      List(new PostTyper) ::                    // Additional checks and cleanups after type checking
      List(new sjs.PrepJSInterop) ::            // Additional checks and transformations for Scala.js (Scala.js only)
      List(new sbt.ExtractAPI) ::               // Sends a representation of the API of classes to sbt via callbacks
      List(new SetRootTree) ::                  // Set the `rootTreeOrProvider` on class symbols
      Nil

  override def newRun(using Context): Run = {
    reset()
    new TASTYRun(this, ctx.addMode(Mode.ReadPositions))
  }

object LDriver extends Driver:
  private val noOut = PrintWriter((_: Int) => ())
  private val noIn  = BufferedReader(InputStreamReader(() => -1))

  private val cpArgs: Array[String] = {
    val sources = List(scala.Tuple.getClass, scala.Float.getClass, core.getClass)
      .map(_.getProtectionDomain.getCodeSource.getLocation.toURI.getPath)

    Array("-classpath", sources.mkString(classpathSeparator))
  }

  override def sourcesRequired: Boolean = false

  private def compileAndExecute[Err, A](
      compiler: LCompiler,
      run: Path => Either[Err, A]
  ): Either[Err | CompileError, A] =
    val outDir = Files.createTempDirectory("scala3-expression")
    outDir.toFile.deleteOnExit()

    setup(cpArgs, initCtx.fresh) match
      case Some((_, rootCtx)) =>
        given Context = rootCtx.fresh
          .setSetting(rootCtx.settings.outputDir, PlainDirectory(Directory(outDir)))
          .setReporter(ConsoleReporter(noIn, noOut))

        val source = SourceFile.virtual("expression", "")
        compiler.newRun.compileSources(source :: Nil)

        ctx.reporter.allErrors
          .map(_.msg)
          .toVector
          .toNev
          .map(x => Compile(x.map(Compile.msgFromDotty)))
          .toLeft {
            val res = run(outDir)
            deleteFile(outDir.toFile)
            res
          }
          .flatten
      case None => Left(Internal(new Exception("Unable to setup context")))

  private def runDir(dir: Path, outputType: LType, arguments: Arguments): Either[errors.Runtime, outputType.Repr] =
    Try {
      val cl     = URLClassLoader(Array(dir.toUri.toURL), this.getClass.getClassLoader)
      val cls    = cl.loadClass(calcCls)
      val method = cls.getMethod(LTranslator.funcName, arguments.getClass)
      method.invoke(null, arguments).asInstanceOf[outputType.Repr]
    }.recoverWith { case ex: InvocationTargetException =>
      Failure(ex.getTargetException)
    }.toEither.left.map(errors.Runtime.apply)

  def compileAndRun(
                     term: LTree,
                     outputType: LType,
                     arguments: Arguments
  ): Either[CompileAndRunError, outputType.Repr] =
    compileAndExecute(
      LCompiler(term, outputType),
      outDir => runDir(outDir, outputType, arguments)
    )

  def compile(term: LTree, outputType: LType): Either[CompileError, ByteCode] =
    compileAndExecute(
      LCompiler(term, outputType),
      outDir =>
        val classByteCode  = Files.readAllBytes(outDir.resolve(s"${LTranslator.calcCls}.class"))
        val objectByteCode = Files.readAllBytes(outDir.resolve(s"${LTranslator.calcCls}$$.class"))
        Right(ByteCode(classByteCode, objectByteCode))
    )

  def run(code: ByteCode, outputType: LType, arguments: Arguments): Either[RunError, outputType.Repr] =
    Try {
      val tmpDir        = Files.createTempDirectory("run-expression")
      val calcClassFile = tmpDir.resolve(s"${LTranslator.calcCls}.class")
      calcClassFile.toFile.createNewFile()
      val calcObjectFile = tmpDir.resolve(s"${LTranslator.calcCls}$$.class")
      calcObjectFile.toFile.createNewFile()
      Using.resource(new BufferedOutputStream(new FileOutputStream(calcClassFile.toFile)))(_.write(code.classCode))
      Using.resource(new BufferedOutputStream(new FileOutputStream(calcObjectFile.toFile)))(_.write(code.objectCode))
      val res = runDir(tmpDir, outputType, arguments)
      deleteFile(tmpDir.toFile)
      res
    }.toEither.left.map(Internal.apply).flatten

// TODO: put term to custom context
class LTranslator(term: LTree, outputType: LType) extends Phase:
  override def phaseName: String    = LTranslator.name
  override def description: String  = LTranslator.description
  override def isCheckable: Boolean = false

  private val sp = Spans.Span(0)

  def translate(using Context) = monitor("translator") {
    val unit = ctx.compilationUnit
    val expr = Translator(term)

    val packName   = Ident("<empty>".mkTermName)
    val calcMod    = calcCls.mkTermName
    val calcMethod = funcName.mkTermName

    val empty = Trees.genericEmptyTree

    val init = DefDef("<init>".mkTermName, List(), TypeTree(), empty)
    val self = Trees.genericEmptyValDef

    val usingT = ValDef(
      Names.EmptyTermName,
      Ident("Arguments".mkTypeName),
      Ident("args".mkTermName)
    ).withAddedFlags(Flags.Given)

    val mainM = DefDef(
      calcMethod,
      List(
        List(
          ValDef("args".mkTermName, Ident("Arguments".mkTypeName), Trees.genericEmptyTree).withFlags(SyntheticTermParam)
        )
      ),
      Translator.translateType(outputType),
      Block(List(usingT), expr).withSpan(sp)
    )

    val dhLang  = Select(Ident("dh".mkTermName), "lang".mkTermName)
    val prelude = Select(dhLang, "prelude".mkTermName)
    val core    = Select(dhLang, "core".mkTermName)

    val tree = PackageDef(
      packName,
      List(prelude, core).map(
        Import(_, List(ImportSelector(Ident("_".mkTermName), empty, empty)))
      ) ++
        List(
          Import(
            Select(core, "RedYellowGreen".mkTermName),
            List(ImportSelector(Ident("_".mkTermName), empty, empty))
          ),
          Import(
            Select(Ident("java".mkTermName), "time".mkTermName),
            List(
              ImportSelector(Ident("LocalDate".mkTermName), empty, empty),
              ImportSelector(Ident("ZonedDateTime".mkTermName), empty, empty)
            )
          ),
          Import(
            Select(Select(Ident("math".mkTermName), "Ordering".mkTermName), "Implicits".mkTermName),
            List(
              ImportSelector(Ident("infixOrderingOps".mkTermName), empty, empty)
            )
          ),
          ModuleDef(calcMod, Template(init, List(), List(), self, mainM :: Nil))
        )
    ).withSpan(sp)

    unit.untpdTree = tree
  }

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] = {
    val unitContexts =
      for unit <- units yield
        report.inform(s"translate ${unit.source}")
        ctx.fresh.setCompilationUnit(unit).withRootImports

    translate(using unitContexts.head)
    record("parsedTrees", ast.Trees.ntrees)

    unitContexts.map(_.compilationUnit)
  }

  def run(using Context): Unit = unsupported("run")

object LTranslator:
  val name: String        = "translator"
  val description: String = "translate to untpd tree"
  val funcName: String    = "run"
  val calcCls: String     = "calculation"

object errors:
  type CompileError       = Compile | Internal
  type RunError           = Internal | Runtime
  type CompileAndRunError = Compile | Internal | Runtime

  case class Runtime(ex: Throwable)
  case class Internal(ex: Throwable)
  case class Compile(errs: NonEmptyVector[Compile.ErrorMessage])

  object Compile:
    private def dropAnsiColors(str: String): String =
      str.replaceAll("\u001B\\[[;\\d]*[ -/]*[@-~]", "")

    def msgFromDotty(msg: Message): ErrorMessage = {
      val kind = msg.kind match
        case DottyMsgKind.TypeMismatch => MsgKind.TypeMismatch
        case DottyMsgKind.NotFound     => MsgKind.NotFound
        case x                         => MsgKind.Other(x.toString)

      ErrorMessage(dropAnsiColors(msg.message), kind)
    }

    case class ErrorMessage(msg: String, kind: MsgKind)

    enum MsgKind:
      case TypeMismatch, NotFound
      case Other(str: String)
