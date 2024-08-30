//> using dep io.circe::circe-core:0.14.6
//> using dep io.github.iltotore::iron:2.3.0-RC2
//> using dep io.7mind.izumi::logstage-core:1.1.0
//> using dep com.softwaremill.sttp.tapir::tapir-core:1.8.2
//> using dep com.github.pureconfig::pureconfig-core:0.17.4
//> using dep org.typelevel::cats-core:2.10.0

import java.util.UUID
import io.circe.{Decoder, Encoder}
import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.string.*
import io.github.iltotore.iron.constraint.numeric.*
import io.github.iltotore.iron.constraint.any
import io.github.iltotore.iron
import io.github.iltotore.iron.constraint.collection.{Empty, MaxLength}
import pureconfig.generic.derivation.default.*
import pureconfig.{ConfigReader, ConfigSource}
import logstage.LogstageCodec
import sttp.tapir.Schema
import scala.compiletime.summonInline
import scala.compiletime.erasedValue

trait NewTypeBase[A]:
  opaque type T <: A = A
  def apply(s: A): T = s

abstract class UUIDNewType(entityName: String) extends NewTypeBase[UUID]:
  given Encoder[T] = Encoder[UUID].contramap(x => x)
  given Decoder[T] = Decoder[UUID].map(apply)
  given LogstageCodec[T] = LogstageCodec[String].contramap(_.toString)
  given Schema[T] = summon[Schema[UUID]].description(s"UUID identifier of $entityName user").as

//----------------------------------------------------

trait IronRefinedType[B, C](using r: RuntimeConstraint[B, C]):
  type R = B :| C
  opaque type T >: R <: R = R

  def apply(x: B): Either[String, T] = Either.cond(r.test(x), x.asInstanceOf[T], r.message)

  given (using Encoder[B]): Encoder[T] = Encoder[B].contramap(x => x)
  given (using LogstageCodec[B]): LogstageCodec[T] = LogstageCodec[B].contramap(x => x)
  given (using Decoder[B]): Decoder[T] = Decoder[B].emap(apply)
  given (using s: Schema[B]): Schema[T] = s.description(r.message).as

//----------------------------------------------------

type EmailRegex = "email-regex"
type NameMaxLength = "name-max-length"

type Keys = EmailRegex | NameMaxLength
type KeyMap[K <: Keys] = K match
  case EmailRegex => String
  case NameMaxLength => Int

case class Config(nameMaxLength: Int, emailRegex: String) derives ConfigReader:
  inline def get[K <: Keys]: KeyMap[K] =
    inline erasedValue[K] match
      case _: EmailRegex => emailRegex
      case _: NameMaxLength => nameMaxLength

//----------------------------------------------------

trait HasConfig[Config]:
  def get: Config

given HasConfig[Config] with
  lazy val get: Config = ConfigSource.file("application.conf").loadOrThrow[Config]

final class RegexConfig[S <: Keys]

given [K <: Keys](using v: ValueOf[K], h: HasConfig[Config]): Constraint[String, RegexConfig[K]] with
  override inline def test(value: String): Boolean =
    value.matches(summonInline[KeyMap[K] =:= String].apply(h.get.get[K]))

  override inline def message: String = s"Should match regex by key ${v.value}"

//----------------------------------------------------

type Name = Name.T
object Name extends IronRefinedType[String, Not[Empty] & Trimmed & MaxLength[100]]

type Email = Email.T
object Email extends IronRefinedType[String, RegexConfig[EmailRegex]]

type ChildId = ChildId.T
object ChildId extends UUIDNewType("Child")

type AdultId = AdultId.T
object AdultId extends UUIDNewType("Adult")

case class Adult(id: AdultId, name: Name, email: Option[Email], children: Vector[ChildId])
  derives Encoder.AsObject, Decoder

case class Child(id: ChildId, name: Name) derives Encoder.AsObject, Decoder

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

//----------------------------------------------------

type Age = Age.T
object Age extends IronRefinedType[Int, GreaterEqual[0]]

type ChildAge = ChildAge.T
object ChildAge extends IronRefinedType[Int, GreaterEqual[0] & Less[18]]

type AdultAge = AdultAge.T
object AdultAge extends IronRefinedType[Int, GreaterEqual[18]]

//----------------------------------------------------

import scala.compiletime.ops.int.>=

given [N <: Int, M <: Int](using (N >= M) =:= true): Implication[GreaterEqual[N], GreaterEqual[M]] = Implication()

summon[Implication[GreaterEqual[0] & Less[18], GreaterEqual[0]]] // compiles
summon[Implication[GreaterEqual[18], GreaterEqual[0]]] // not compiles

val c: ChildAge = 16
c: Age // compiles

val a: AdultAge = 42
a: Age // compiles
