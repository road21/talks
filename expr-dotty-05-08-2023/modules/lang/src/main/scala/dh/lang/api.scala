package dh.lang

import dh.lang.core.{AnyCalc, Arguments, CalcType}
import dh.lang.data.{ByteCode, RefId}
import cats.Monad
import cats.parse.Parser
import dh.lang.errors.{CompileAndRunError, CompileError, RunError}
import dh.lang.parser

object api:
  def parse(code: String): Either[Parser.Error, Term] = parser.parse(code)

  def link[F[_]: Monad](term: Term)(using Refer[F]): F[linker.Result] = linker[F](term)

  private def mapToArgs(args: Map[RefId, AnyCalc]): Arguments = Arguments(args.map { case (x, y) => (x: String) -> y })

  def compileAndRun(
      ast: Term,
      outType: CalcType,
      args: Map[RefId, AnyCalc]
  ): Either[CompileAndRunError, outType.Repr] =
    LDriver.compileAndRun(ast, outType, mapToArgs(args))

  def compile(ast: Term, outType: CalcType): Either[CompileError, ByteCode] =
    LDriver.compile(ast, outType)

  def run(code: ByteCode, outType: CalcType, args: Map[RefId, AnyCalc]): Either[RunError, outType.Repr] =
    LDriver.run(code, outType, mapToArgs(args))
