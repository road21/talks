package dh.lang.parser

import cats.parse.Parser.char
import cats.parse.{Parser, Parser0}

object ParserOps:
  extension [A](parser: Parser0[A])
    def ?? : Parser0[Option[A]] = parser.?.backtrack.orElse(Parser.pure(None))

  val space   = char(' ')
  val spacing = space.rep0.void

  extension [A](parser: Parser[A])
    def sp: Parser[A] = parser.surroundedBy(spacing)
