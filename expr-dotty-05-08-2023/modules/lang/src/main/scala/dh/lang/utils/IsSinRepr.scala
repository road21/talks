package dh.lang.utils

import dh.lang.core.*

import java.time.{LocalDate, ZonedDateTime}

trait IsSinRepr[A]:
  type T <: CalcType
  def value: T

object IsSinRepr:
  @inline def apply[A](using r: IsSinRepr[A]): IsSinRepr[A] = r

  given IsSinRepr[Boolean] with
    type T = TBool.type
    val value = TBool

  given IsSinRepr[String] with
    type T = TStr.type
    val value = TStr

  given IsSinRepr[BigDecimal] with
    type T = TDecimal.type
    val value = TDecimal

  given IsSinRepr[RedYellowGreen] with
    type T = TRYG.type
    val value = TRYG

  given IsSinRepr[LocalDate] with
    type T = TDate.type
    val value = TDate

  given IsSinRepr[ZonedDateTime] with
    type T = TDateTime.type
    val value = TDateTime
