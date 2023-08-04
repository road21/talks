package dh.lang

import java.time.{LocalDate, ZonedDateTime}

object core:
  // common types
  enum RedYellowGreen:
    case red, yellow, green

    def fold[A](redCase: => A, yellowCase: => A, greenCase: => A): A =
      this match
        case `red`    => redCase
        case `yellow` => yellowCase
        case `green`  => greenCase
  object RedYellowGreen:
    given Ordering[RedYellowGreen] = Ordering.by(_.ordinal)

  // calc types
  sealed trait LType:
    type Repr

  case object TBool extends LType:
    override type Repr = Boolean

  case object TStr extends LType:
    override type Repr = String

  case object TDecimal extends LType:
    override type Repr = BigDecimal

  case object TRYG extends LType:
    override type Repr = RedYellowGreen

  case object TDate extends LType:
    override type Repr = LocalDate
    
  case object TDateTime extends LType:
    override type Repr = ZonedDateTime

  type AnyCalc = TBool.Repr | TStr.Repr | TDecimal.Repr | TRYG.Repr | TDate.Repr | TDateTime.Repr

  object LType:
    type WithRepr[R] = LType { type Repr = R }

  // context
  case class Arguments(calc: Map[String, AnyCalc])
  object Arguments:
    def empty: Arguments = Arguments(Map())

  def calc[Type <: AnyCalc](id: String)(using Arguments): Type =
    summon[Arguments].calc(id).asInstanceOf[Type]

  given Ordering[LocalDate] with
    def compare(x: LocalDate, y: LocalDate): Int =
      x.compareTo(y)

  given Ordering[ZonedDateTime] with
    def compare(x: ZonedDateTime, y: ZonedDateTime): Int =
      x.compareTo(y)