package dh.lang

import java.time.{ZoneId, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.quoted.*
import scala.util.Try

trait datetime:
  inline def toDateTime(inline str: String): ZonedDateTime =
    ${ DateTimeMacro.toDateTimeCode('str) }

  inline def toDateTime(inline str: String, inline format: String): ZonedDateTime =
    ${ DateTimeMacro.toDateTimeCode('str, 'format) }

object DateTimeMacro:
  def toDateTimeCode(str: Expr[String])(using Quotes): Expr[ZonedDateTime] =
    import quotes.reflect.report

    str.value match
      case Some(value) =>
        Try {
          ZonedDateTime.parse(value)
        }.fold(_ => report.errorAndAbort("Expected datetime, got: " + value, str), Expr.apply)
      case None => '{ ZonedDateTime.parse($str) }

  def toDateTimeCode(str: Expr[String], format: Expr[String])(using Quotes): Expr[ZonedDateTime] =
    import quotes.reflect.report

    (for {
      strValue    <- str.value
      formatValue <- format.value
    } yield Try {
      ZonedDateTime.parse(strValue, DateTimeFormatter.ofPattern(formatValue))
    }.fold(_ => report.errorAndAbort(s"Expected datetime in format $formatValue, got: $strValue"), Expr.apply)).getOrElse('{ ZonedDateTime.parse($str, DateTimeFormatter.ofPattern($format)) })

  given ToExpr[ZoneId] with
    def apply(x: ZoneId)(using Quotes): Expr[ZoneId] = '{ ZoneId.of(${Expr(x.getId)}) }

  given ToExpr[ZonedDateTime] with
    def apply(x: ZonedDateTime)(using Quotes): Expr[ZonedDateTime] =
      '{ ZonedDateTime.of(${Expr(x.getYear)}, ${Expr(x.getMonthValue)}, ${Expr(x.getDayOfMonth)}, ${Expr(x.getHour)}, ${Expr(x.getMinute)}, ${Expr(x.getSecond)}, ${Expr(x.getNano)}, ${Expr(x.getZone)}) }
