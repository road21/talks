package dh.lang

import dh.lang.roundMacro.roundCode

import scala.math.BigDecimal.RoundingMode
import scala.quoted.*

trait decimal:
  def abs(arg: BigDecimal): BigDecimal = arg.abs

  def decimal(str: String): BigDecimal =
    BigDecimal(str)

  inline def round(inline arg: BigDecimal, inline scale: BigDecimal): BigDecimal =
    ${ roundMacro.roundCode('arg, 'scale) }

object roundMacro:
  def roundCode(arg: Expr[BigDecimal], scale: Expr[BigDecimal])(using Quotes): Expr[BigDecimal] =
    import quotes.reflect.report

    scale.value match
      case Some(base) if base.isValidInt =>
        val scale = Expr(base.toIntExact)
        '{ $arg.setScale($scale, RoundingMode.HALF_UP) }
      case Some(x) =>
        report.errorAndAbort("Expected integer scale, got: " + x, scale)
      case None =>
        '{ $arg.setScale($scale.toIntExact, RoundingMode.HALF_UP) }

  given FromExpr[BigDecimal] with
    override def unapply(x: Expr[BigDecimal])(using Quotes): Option[BigDecimal] =
      x match
        case '{ BigDecimal($arg: Double) } =>
          arg.value.map(BigDecimal.apply)
        case _ => None
