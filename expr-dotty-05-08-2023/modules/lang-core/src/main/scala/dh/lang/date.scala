package dh.lang

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.quoted.*
import scala.util.Try

trait date:
  inline def toDate(inline str: String): LocalDate =
    ${ DateMacro.toDateCode('str) }

  inline def toDate(inline str: String, inline format: String): LocalDate =
    ${ DateMacro.toDateCode('str, 'format) }

object DateMacro:
  def toDateCode(str: Expr[String])(using Quotes): Expr[LocalDate] =
    import quotes.reflect.report

    str.value match
      case Some(value) =>
        Try {
          LocalDate.parse(value)
        }.fold(_ => report.errorAndAbort("Expected date, got: " + value, str), Expr.apply)
      case None => '{ LocalDate.parse($str) }

  def toDateCode(str: Expr[String], format: Expr[String])(using Quotes): Expr[LocalDate] =
    import quotes.reflect.report

    (for {
      strValue    <- str.value
      formatValue <- format.value
    } yield Try {
      LocalDate.parse(strValue, DateTimeFormatter.ofPattern(formatValue))
    }.fold(_ => report.errorAndAbort(s"Expected date in format $formatValue, got: $strValue"), Expr.apply)).getOrElse('{ LocalDate.parse($str, DateTimeFormatter.ofPattern($format)) })

  given ToExpr[LocalDate] with
    def apply(x: LocalDate)(using Quotes): Expr[LocalDate] =
      '{ LocalDate.ofEpochDay(${Expr(x.toEpochDay)}) }
