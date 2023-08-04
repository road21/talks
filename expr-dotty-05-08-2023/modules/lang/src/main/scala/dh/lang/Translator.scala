package dh.lang

import dotty.tools.dotc.ast.Trees.{Tree, Untyped}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Names
import dotty.tools.dotc.core.Constants.Constant as DConstant
import dotty.tools.dotc.util.{SourceFile, Spans}
import dotty.tools.dotc.ast.untpd.{Apply, Ident, InfixOp, Literal, Select, TypeApply}
import dh.lang.core.{CalcType, TBool, TDate, TDateTime, TDecimal, TRYG, TStr}

object Translator:
  val dummySpan = Spans.Span(0)

  def apply(term: Term)(using SourceFile): Tree[Untyped] =
    translate(term)

  // TODO: make it stack safe
  def translate(term: Term)(using SourceFile): Tree[Untyped] =
    term match {
      case Term.Literal(c) => constant(c)
      case Term.Ident(n)   => Ident(Names.termName(n))
      case Term.InfixOp(l, i, r) =>
        InfixOp(translate(l), ident(i), translate(r))
      case Term.Apply(l, r) =>
        Apply(translate(l), r.map(translate).toList)
      case Term.Select(q, n) =>
        Select(translate(q), Names.termName(n))
      case Term.Calculation(u, t) =>
        Apply(
          TypeApply(Ident("calc".mkTermName), List(translateType(t))),
          List(constant(Constant.PString(u)))
        )
    }

  def translateType(t: CalcType)(using SourceFile): untpd.Tree = {
    val typeName = t match
      case TDecimal  => "BigDecimal"
      case TBool     => "Boolean"
      case TStr      => "String"
      case TRYG      => "RedYellowGreen"
      case TDate     => "LocalDate"
      case TDateTime => "ZonedDateTime"

    Ident(typeName.mkTypeName)
  }

  def ident(ident: Term.Ident)(using SourceFile): Ident =
    Ident(Names.termName(ident.name))

  def constant(cnst: Constant)(using SourceFile): Tree[Untyped] =
    cnst match {
      case Constant.PBoolean(b) => Literal(DConstant(b))
      case Constant.PString(s)  => Literal(DConstant(s))
      case Constant.PDecimal(d) => Apply(Ident("BigDecimal".mkTermName), List(Literal(DConstant(d.toDouble))))
    }
