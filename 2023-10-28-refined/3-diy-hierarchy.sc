//> using dep io.circe::circe-core:0.14.6
//> using dep io.7mind.izumi::logstage-core:1.1.0
//> using dep com.softwaremill.sttp.tapir::tapir-core:1.8.2

import java.util.UUID
import io.circe.{Encoder, Decoder}
import logstage.LogstageCodec
import sttp.tapir.Schema

trait RefinedTo[A, R]:
  def to(a: A): Either[String, R]

object RefinedTo:
  given [A]: RefinedTo[A, A] with
    def to(a: A): Either[String, A] = Right(a)

//----------------------------------------------------

trait RefinedBase[A]:
  opaque type T <: A = A

  def predicates: Vector[(String, A => Boolean)] = Vector()

  def apply(s: A): Either[String, T] =
    val errs = predicates.collect { case (descr, pred) if !pred(s) => descr }
    if (errs.isEmpty) Right(s)
    else Left("Following predicates are not hold: " + errs.mkString(", "))

  given RefinedTo[A, T] with
    def to(a: A): Either[String, T] = apply(a)

//----------------------------------------------------

trait IntRefined[A <: Int](using R: RefinedTo[Int, A]) extends RefinedBase[A]:
  override def apply(a: Int): Either[String, T] = R.to(a).flatMap(super.apply)

  lazy val description: String =
    "String that satisfies predicates: " + predicates.map(_._1).mkString(", ")

  given Encoder[T] = Encoder[Int].contramap(x => x)
  given Decoder[T] = Decoder[Int].emap(apply)
  given LogstageCodec[T] = LogstageCodec[Int].contramap(x => x)
  given Schema[T] = summon[Schema[String]].description(description).as

//----------------------------------------------------

trait Min[A <: Int](value: Int) extends IntRefined[A]:
  override def predicates: Vector[(String, A => Boolean)] =
    super.predicates.prepended(
      (s"less or eq than $value", _ >= value)
    )

trait Max[A <: Int](value: Int) extends IntRefined[A]:
  override def predicates: Vector[(String, A => Boolean)] =
    super.predicates.prepended(
      (s"greater or eq than $value", _ <= value)
    )

//----------------------------------------------------

type Age = Age.T
object Age extends Min[Int](0)

type ChildAge = ChildAge.T
object ChildAge extends Max[Age](17)

type AdultAge = AdultAge.T
object AdultAge extends Min[Age](18)

summon[ChildAge <:< Age]
summon[ChildAge <:< Int]
summon[AdultAge <:< Age]

println(AdultAge(-10))
println(AdultAge(100))
println(AdultAge(10))
println(ChildAge(100))
println(ChildAge(10))
