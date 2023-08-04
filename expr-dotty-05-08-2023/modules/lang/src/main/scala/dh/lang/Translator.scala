package dh.lang

import dotty.tools.dotc.ast.Trees.{Tree, Untyped}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Constants.Constant as DConstant
import dotty.tools.dotc.util.{SourceFile, Spans}
import dotty.tools.dotc.ast.untpd.{Apply, Ident, InfixOp, Literal, Select, TypeApply}
import dh.lang.core.{LType, TBool, TDate, TDateTime, TDecimal, TRYG, TStr}

object Translator:
  val dummySpan = Spans.Span(0)

  def apply(term: LTree)(using SourceFile): Tree[Untyped] =
    translate(term)

  // TODO: make it stack safe
  def translate(term: LTree)(using SourceFile): Tree[Untyped] =
    term match {
      case LTree.Literal(c) => constant(c)
      case LTree.Ident(n)   => Ident(Names.termName(n))
      case LTree.InfixOp(l, i, r) =>
        InfixOp(translate(l), ident(i), translate(r))
      case LTree.Apply(l, r) =>
        Apply(translate(l), r.map(translate).toList)
      case LTree.Select(q, n) =>
        Select(translate(q), Names.termName(n))
      case LTree.Calculation(u, t) =>
        Apply(
          TypeApply(Ident("calc".mkTermName), List(translateType(t))),
          List(constant(Constant.PString(u)))
        )
    }

  def translateType(t: LType)(using SourceFile): untpd.Tree = {
    val typeName = t match
      case TDecimal  => "BigDecimal"
      case TBool     => "Boolean"
      case TStr      => "String"
      case TRYG      => "RedYellowGreen"
      case TDate     => "LocalDate"
      case TDateTime => "ZonedDateTime"

    Ident(typeName.mkTypeName)
  }

  def ident(ident: LTree.Ident)(using SourceFile): Ident =
    Ident(Names.termName(ident.name))

  def constant(cnst: Constant)(using SourceFile): Tree[Untyped] =
    cnst match {
      case Constant.PBoolean(b) => Literal(DConstant(b))
      case Constant.PString(s)  => Literal(DConstant(s))
      case Constant.PDecimal(d) => Apply(Ident("BigDecimal".mkTermName), List(Literal(DConstant(d.toDouble))))
    }
