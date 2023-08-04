package dh.lang.parser

import cats.data.NonEmptyVector
import cats.parse.Numbers.signedIntString
import cats.parse.Parser.*
import cats.parse.strings.Json.delimited
import cats.parse.{Numbers, Parser, Parser0, Rfc5234}
import dh.lang.data.Identifier
import dh.lang.data.Identifier.*
import dh.lang.parser.parse.Fragment.*
import dh.lang.Term
import dh.lang.Term.*
import dh.lang.Constant.*
import scala.util.Try
import ParserOps.*

object parse:
  val lowerLetter = charIn('a' to 'z')
  val upperLetter = charIn('A' to 'Z')
  val underscore  = char('_')

  val letter        = lowerLetter orElse upperLetter orElse underscore
  val letterOrDigit = letter orElse Rfc5234.digit

  val identifier = (letter *> letterOrDigit.rep0).string

  val bool = ((string("true") as true) | (string("false") as false)).map(x => Literal(PBoolean(x)))
  val str  = delimited.parser.map(x => Literal(PString(x)))
  val decimalString = (signedIntString ~ (Parser.char('.') ~ Numbers.digits).?? ~
    (Parser.charIn("eE") ~ Parser.charIn("+-").?? ~ Numbers.digits).??).string
  val decimal = decimalString.mapFilter(x => Try(BigDecimal(x)).map(b => Literal(PDecimal(b))).toOption)

  val const = bool | str | decimal

  val ident = identifier.map(Identifier.apply).map(Term.Ident.apply)

  val simpleTerm = const | ident

  val operatorChars = charIn("*/%+-:<>=!&^|$@#")
  val operatorName  = ((letter | operatorChars) *> (letterOrDigit | operatorChars).rep0).string

  def opP(op: String | Char): Parser[String] =
    op match
      case ch: Char    => char(ch).as(ch.toString)
      case str: String => string(str).as(str)

  val opPrec = NonEmptyVector
    .of(
      opP('*') | opP('/') | opP('%'),
      opP('+') | opP('-'),
      opP("<=") | opP(">=") | opP('>') | opP('<'),
      opP("==") | opP("!="),
      opP("&&") | opP("and"),
      opP("||") | opP("or")
    )
    .map(_.sp)

  val otherOps =
    Parser.product01(opPrec.map(not).reduce(_ *> _), operatorName.sp).map(_._2)

  enum Fragment:
    case SelectFragment(identifier: String)
    case ApplyFragment(arguments: List[Term])
    case InfixOpFragment(op: String, term: Term)

  type FragPars = Parser[Fragment]

  def fragmentsToTerm(t: Term, fs: List[Fragment]): Term =
    fs.foldLeft(t)((acc, t) =>
      t match
        case SelectFragment(i)      => Select(acc, i.mkIdent)
        case ApplyFragment(arg)     => Apply(acc, arg.toVector)
        case InfixOpFragment(op, t) => InfixOp(acc, Ident(op.mkIdent), t)
    )

  val term: Parser[Term] = Parser.recursive(term =>
    val bracketTerm = char('(') *> term.sp <* char(')').sp

    val selectFrag: FragPars = (char('.').sp *> identifier.sp).map(SelectFragment.apply)
    val applyFrag: FragPars =
      (char('(') *> term.sp.repSep0(string(",")) <* char(')')).map(ApplyFragment.apply)

    val termBr   = (simpleTerm | bracketTerm).sp
    val noOpTerm = (termBr ~ (selectFrag | applyFrag).sp.rep0).map(fragmentsToTerm)

    val infix =
      (t: Parser[Term], op: Parser[String]) => (t ~ (op ~ t).map(InfixOpFragment.apply).rep0).map(fragmentsToTerm)

    opPrec.foldLeft(infix(noOpTerm, otherOps))(infix)
  )

  def apply(expr: String): Either[Parser.Error, Term] = term.parseAll(expr)
