package dh.lang

import cats.Hash
import cats.derived.*
import cats.syntax.hash.*
import dh.lang.Constant.PString
import dh.lang.LTree.{Apply, Ident, InfixOp, Literal, Select}
import dh.lang.core.{LType, RedYellowGreen}
import dh.lang.data.{Identifier, RefId}
import dh.lang.data.Identifier.mkIdent
import io.circe.Codec

import java.time.LocalDate

enum Constant derives Hash:
  case PBoolean(b: Boolean)
  case PString(s: String)
  case PDecimal(d: BigDecimal)

enum LTree derives Hash:
  case Literal(p: Constant)
  case Apply(l: LTree, r: Vector[LTree])
  case Ident(name: Identifier)
  case InfixOp(left: LTree, op: Ident, right: LTree)
  case Select(qualifier: LTree, name: Identifier)
  case Calculation(id: RefId, calcType: LType)

object LTree:
  extension (t: LTree)
    def apply(arg: LTree*): LTree                  = LTree.Apply(t, arg.toVector)
    def infix(op: Identifier, right: LTree): LTree = LTree.InfixOp(t, LTree.Ident(op), right)
    def select(name: Identifier): LTree           = LTree.Select(t, name)

  extension (n: BigDecimal) def t: LTree = LTree.Literal(Constant.PDecimal(n))
  extension (i: Int)
    def t: LTree        = BigDecimal(i).t
    def bd: BigDecimal = BigDecimal(i)

  extension (l: Long)
    def t: LTree        = BigDecimal(l).t
    def bd: BigDecimal = BigDecimal(l)

  extension (b: Boolean) def t: LTree = LTree.Literal(Constant.PBoolean(b))

  extension (s: String)
    def t: LTree           = LTree.Literal(Constant.PString(s))
    def ident: LTree.Ident = LTree.Ident(s.mkIdent)

  extension (r: RefId) def calc(tp: LType): LTree = LTree.Calculation(r, tp)

  extension (f: Float) def t: LTree = f.toDouble.t

  extension (d: Double)
    def t: LTree        = LTree.Literal(Constant.PDecimal(bd))
    def bd: BigDecimal = BigDecimal(d)

  extension (ryg: RedYellowGreen) def t: LTree = ryg.toString.ident

  extension (date: LocalDate) def t: LTree = date.toString.ident

  extension (refId: RefId) def calcr(tp: LType): LTree = LTree.Calculation(refId, tp)
