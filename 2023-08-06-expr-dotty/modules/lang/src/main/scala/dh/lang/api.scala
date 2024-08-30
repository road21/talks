package dh.lang

import dh.lang.core.{AnyCalc, Arguments, LType}
import dh.lang.data.{ByteCode, RefId}
import cats.Monad
import cats.parse.Parser
import dh.lang.errors.{CompileAndRunError, CompileError, RunError}
import dh.lang.parser

object api:
  def parse(code: String): Either[Parser.Error, LTree] = parser.parse(code)

  def link[F[_]: Monad](term: LTree)(using Refer[F]): F[linker.Result] = linker[F](term)

  private def mapToArgs(args: Map[RefId, AnyCalc]): Arguments = Arguments(args.map { case (x, y) => (x: String) -> y })

  def compileAndRun(
                     ast: LTree,
                     outType: LType,
                     args: Map[RefId, AnyCalc]
  ): Either[CompileAndRunError, outType.Repr] =
    LDriver.compileAndRun(ast, outType, mapToArgs(args))

  def compile(ast: LTree, outType: LType): Either[CompileError, ByteCode] =
    LDriver.compile(ast, outType)

  def run(code: ByteCode, outType: LType, args: Map[RefId, AnyCalc]): Either[RunError, outType.Repr] =
    LDriver.run(code, outType, mapToArgs(args))
