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
  sealed trait CalcType:
    type Repr

  case object TBool extends CalcType:
    override type Repr = Boolean

  case object TStr extends CalcType:
    override type Repr = String

  case object TDecimal extends CalcType:
    override type Repr = BigDecimal

  case object TRYG extends CalcType:
    override type Repr = RedYellowGreen

  case object TDate extends CalcType:
    override type Repr = LocalDate
    
  case object TDateTime extends CalcType:
    override type Repr = ZonedDateTime

  type AnyCalc = TBool.Repr | TStr.Repr | TDecimal.Repr | TRYG.Repr | TDate.Repr | TDateTime.Repr

  object CalcType:
    type WithRepr[R] = CalcType { type Repr = R }

  // context
  case class Arguments(calc: Map[String, AnyCalc])
  object Arguments:
    def empty: Arguments = Arguments(Map())

  def calc[Type <: AnyCalc](id: String)(using Arguments): Type =
    summon[Arguments].calc(id).asInstanceOf[Type]
