//> using dep io.circe::circe-core:0.14.6
//> using dep io.7mind.izumi::logstage-core:1.1.0
//> using dep com.softwaremill.sttp.tapir::tapir-core:1.8.2
//> using dep com.github.pureconfig::pureconfig-core:0.17.4
//> using dep org.typelevel::cats-core:2.10.0

import java.util.UUID
import io.circe.{Encoder, Decoder}
import logstage.LogstageCodec
import sttp.tapir.Schema
import pureconfig.generic.derivation.default.*
import pureconfig.{ConfigReader, ConfigSource}

trait RefinedBase[A]:
  opaque type T <: A = A

  def predicates: Vector[(String, A => Boolean)] = Vector()

  def apply(s: A): Either[String, T] =
    val errs = predicates.collect { case (descr, pred) if !pred(s) => descr }
    if (errs.isEmpty) Right(s)
    else Left("Following predicates are not hold: " + errs.mkString(", "))

//----------------------------------------------------

trait StringRefined extends RefinedBase[String]:
  lazy val description: String =
    "String that satisfies predicates: " + predicates.map(_._1).mkString(", ")

  given Encoder[T] = Encoder[String].contramap(x => x)
  given Decoder[T] = Decoder[String].emap(apply)
  given LogstageCodec[T] = LogstageCodec[String].contramap(x => x)
  given Schema[T] = summon[Schema[String]].description(description).as

trait IntRefined extends RefinedBase[Int]:
  given Encoder[T] = Encoder[Int].contramap(x => x)
  given Decoder[T] = Decoder[Int].emap(apply)

//----------------------------------------------------

trait NewTypeBase[A]:
  opaque type T <: A = A
  def apply(s: A): T = s

abstract class UUIDNewType(entityName: String) extends NewTypeBase[UUID]:
  given Encoder[T] = Encoder[UUID].contramap(x => x)
  given Decoder[T] = Decoder[UUID].map(apply)
  given LogstageCodec[T] = LogstageCodec[String].contramap(_.toString)
  given Schema[T] = summon[Schema[UUID]].description(s"UUID identifier of $entityName user").as

//----------------------------------------------------

trait Regex(pattern: String) extends StringRefined:
  val regex = pattern.r
  override def predicates: Vector[(String, String => Boolean)] =
    super.predicates.prepended(s"matches regex $pattern" -> regex.matches)

trait NonEmpty extends StringRefined:
  override def predicates: Vector[(String, String => Boolean)] =
    super.predicates.prepended("non empty" -> (_.nonEmpty))

trait Trimmed extends StringRefined:
  override def predicates: Vector[(String, String => Boolean)] =
    super.predicates.prepended("trimmed" ->
      (x => !x.headOption.contains(' ') && !x.lastOption.contains(' '))
    )

trait MaxLength(length: Int) extends StringRefined:
  override def predicates: Vector[(String, String => Boolean)] =
    super.predicates.prepended(s"not longer than $length" -> (_.length <= length))

trait HasConfig[Config]:
  def get: Config

trait RegexConfig[Config](key: Config => String)(using conf: HasConfig[Config]) extends StringRefined:
  lazy val regex = key(conf.get).r

  override def predicates: Vector[(String, String => Boolean)] =
    super.predicates.prepended(
      s"matches regex $regex" -> regex.matches
    )

trait MaxLengthConfig[Config](key: Config => Int)(using conf: HasConfig[Config]) extends StringRefined:
  lazy val maxLength = key(conf.get)
  override def predicates: Vector[(String, String => Boolean)] =
    super.predicates.prepended(s"not longer than $maxLength" -> (_.length <= maxLength))

//----------------------------------------------------

object StringPredicates:
  val nonEmpty: (String, String => Boolean) = ("non empty", _.nonEmpty)
  val trimmed: (String, String => Boolean) =
    ("trimmed", x => !x.headOption.contains(' ') && !x.lastOption.contains(' '))

  def maxLength(n: Int): (String, String => Boolean) =
    (s"not longer than $n", _.length <= n)
  def matches(regex: String): (String, String => Boolean) =
    (s"matches regex $regex", _.matches(regex))

//----------------------------------------------------

import StringPredicates.*

case class Config(emailRegex: String, nameMaxLength: Int) derives ConfigReader

given HasConfig[Config] with
  lazy val get: Config = ConfigSource.file("application.conf").loadOrThrow[Config]

//----------------------------------------------------

type Name = Name.T
object Name extends NonEmpty, Trimmed, MaxLengthConfig[Config](_.nameMaxLength)

type Email = Email.T
object Email extends RegexConfig[Config](_.emailRegex)

type ChildId = ChildId.T
object ChildId extends UUIDNewType("Child")

type AdultId = AdultId.T
object AdultId extends UUIDNewType("Adult")

case class Adult(id: AdultId, name: Name, email: Option[Email], children: Vector[ChildId])
  derives Encoder.AsObject, Decoder

case class Child(id: ChildId, name: Name)
  derives Encoder.AsObject, Decoder

//----------------------------------------------------

import io.circe.syntax.*
import cats.syntax.apply.*

println(Email("am@@r"))
println(Email("amtroitskiy@gmail.com"))

println(Name(""))
println(Name(" LololoLololoLololoLololoLololoLololoLololoLololoLololoLololoLololoLololoLololoLololololoLololoLololo"))
println(Name("Aleksey"))

for
  p <- Name("Петя").map(Child(ChildId(UUID.randomUUID()), _))
  s <- Name("Саша").map(
    Adult(
      AdultId(UUID.randomUUID()), _, None, Vector(p.id)
    )
  )
  l <- (Name("Леша"), Email("amtroitskiy@gmail.com")).mapN(
    (name, email) => Adult(AdultId(UUID.randomUUID()), name, Some(email), Vector(p.id))
  )
yield
  println(p.asJson)
  println(s.asJson)
  println(l.asJson)
