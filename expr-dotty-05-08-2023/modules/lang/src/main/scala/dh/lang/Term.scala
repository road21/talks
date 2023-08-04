package dh.lang

import cats.Hash
import cats.derived.*
import cats.syntax.hash.*
import dh.lang.Constant.PString
import dh.lang.Term.{Apply, Ident, InfixOp, Literal, Select}
import dh.lang.core.{CalcType, RedYellowGreen}
import dh.lang.data.{Identifier, RefId}
import dh.lang.data.Identifier.mkIdent
import io.circe.Codec

import java.time.LocalDate

enum Constant derives Hash:
  case PBoolean(b: Boolean)
  case PString(s: String)
  case PDecimal(d: BigDecimal)

enum Term derives Hash:
  case Literal(p: Constant)
  case Apply(l: Term, r: Vector[Term])
  case Ident(name: Identifier)
  case InfixOp(left: Term, op: Ident, right: Term)
  case Select(qualifier: Term, name: Identifier)
  case Calculation(id: RefId, calcType: CalcType)

object Term:
  extension (t: Term)
    def apply(arg: Term*): Term                  = Term.Apply(t, arg.toVector)
    def infix(op: Identifier, right: Term): Term = Term.InfixOp(t, Term.Ident(op), right)
    def select(name: Identifier): Term           = Term.Select(t, name)

  extension (n: BigDecimal) def t: Term = Term.Literal(Constant.PDecimal(n))
  extension (i: Int)
    def t: Term        = BigDecimal(i).t
    def bd: BigDecimal = BigDecimal(i)

  extension (l: Long)
    def t: Term        = BigDecimal(l).t
    def bd: BigDecimal = BigDecimal(l)

  extension (b: Boolean) def t: Term = Term.Literal(Constant.PBoolean(b))

  extension (s: String)
    def t: Term           = Term.Literal(Constant.PString(s))
    def ident: Term.Ident = Term.Ident(s.mkIdent)

  extension (r: RefId) def calc(tp: CalcType): Term = Term.Calculation(r, tp)

  extension (f: Float) def t: Term = f.toDouble.t

  extension (d: Double)
    def t: Term        = Term.Literal(Constant.PDecimal(bd))
    def bd: BigDecimal = BigDecimal(d)

  extension (ryg: RedYellowGreen) def t: Term = ryg.toString.ident

  extension (date: LocalDate) def t: Term = date.toString.ident

  extension (refId: RefId) def calcr(tp: CalcType): Term = Term.Calculation(refId, tp)
