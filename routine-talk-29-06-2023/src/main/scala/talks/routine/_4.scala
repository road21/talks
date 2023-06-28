package talks.routine

import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}
import cats.{Applicative, MonadThrow}
import io.circe.{Decoder, Encoder}

import scala.deriving.Mirror
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.traverse.*
import talks.routine._3.{Action, ActionType, Field, FieldType, Form, LKey, Lang, Localizer, localizeForm}

import java.time.LocalDate

// Локализация и подстановка дефолтных значений через HKD
object _4 extends IOApp:
  type Text = Text.T

  object Text extends TextInstances:
    opaque type T <: String = String
    def apply(key: String): T = key

  trait TextInstances:
    given Encoder[Text] = Encoder[String].contramap(x => x)

  enum Lang:
    case RU, EN

  type LKey = LKey.K

  object LKey extends LKeyInstances:
    opaque type K <: String = String
    def apply(key: String): K = key

  trait LKeyInstances:
    given Encoder[LKey] = Encoder[String].contramap(x => x)

  enum FieldType:
    case string, date

  object FieldType:
    given Encoder[FieldType] = Encoder[String].contramap(_.toString)

  enum ActionType:
    case send

  object ActionType:
    given Encoder[ActionType] = Encoder[String].contramap(_.toString)

  enum FieldValue derives Encoder.AsObject, Decoder:
    case string(value: String)
    case date(value: LocalDate)

  type Localize
  type Context

  trait TraverseK[U[_[_]]]:
    def traverseK[A[_], B[_], F[_] : Applicative](ca: U[A])(f: [X] => A[X] => F[B[X]]): F[U[B]]

  case class Field[F[_]](
    title: F[Localize],
    hint: F[Localize],
    default: F[Context],
    `type`: FieldType
  )

  object Field:
    given TraverseK[Field] = new TraverseK[Field]:
      override def traverseK[A[_], B[_], F[_] : Applicative](ca: Field[A])(f: [X] => A[X] => F[B[X]]): F[Field[B]] =
        (f(ca.title), f(ca.hint), f(ca.default)).mapN { (t, h, d) => Field(t, h, d, ca.`type`) }

    given templateEnc: Encoder.AsObject[Field[Template]] = Encoder.AsObject.derived
    given renderedEnc: Encoder.AsObject[Field[Rendered]] = Encoder.AsObject.derived

  case class Action[F[_]](
    id: String,
    title: F[Localize],
    `type`: ActionType
  )

  object Action:
    given TraverseK[Action] = new TraverseK[Action]:
      override def traverseK[A[_], B[_], F[_]: Applicative](ca: Action[A])(f: [X] => A[X] => F[B[X]]): F[Action[B]] =
        f(ca.title).map(Action(ca.id, _, ca.`type`))

  case class Form[F[_]](
    title: F[Localize],
    fields: List[Field[F]],
    actions: List[Action[F]]
  )

  object Form:
    given TraverseK[Form] = new TraverseK[Form]:
      override def traverseK[A[_], B[_], F[_]: Applicative](ca: Form[A])(f: [X] => A[X] => F[B[X]]): F[Form[B]] =
        (
          f(ca.title),
          ca.fields.traverse(summon[TraverseK[Field]].traverseK(_)(f)),
          ca.actions.traverse(summon[TraverseK[Action]].traverseK(_)(f))
        ).mapN((t, fs, as) => Form(t, fs, as))

  enum Template[A]:
    case L(key: LKey) extends Template[Localize]
    case C(default: Option[UserProp]) extends Template[Context]

  enum Rendered[A]:
    case L(text: Text) extends Rendered[Localize]
    case C(default: Option[FieldValue]) extends Rendered[Context]

  trait Localizer[F[_]]:
    def localize(lang: Lang, key: LKey): F[Text]

  object Localizer:
    val keys: Map[LKey, (Text, Text)] = Map(
      "name_form_title" -> ("Введите свои данные", "Enter personal info"),
      "first_name_field_title" -> ("Имя", "First name"),
      "last_name_field_title" -> ("Фамилия", "Last name"),
      "name_hint" -> ("Мой ответ", "My answer"),
      "birth_date_field_title" -> ("Дата рождения", "Date of birth"),
      "birth_date_hint" -> ("Дата", "Date"),
      "confirm_action_title" -> ("Подтвердить", "Confirm"),
    ).map { case (k, (en, ru)) => LKey(k) -> (Text(en), Text(ru)) }

    def make[F[_] : MonadThrow]: Localizer[F] = (lang: Lang, key: LKey) =>
      keys.get(LKey(key)).fold {
        MonadThrow[F].raiseError(new Exception(s"no translation for key $key"))
      } { case (ru, en) =>
        Text(
          lang match
            case Lang.RU => ru
            case Lang.EN => en
        ).pure[F]
      }

  enum UserProp:
    case birthDate, firstName, lastName

  case class UserInfo(
    firstName: Option[String],
    lastName: Option[String],
    birthDate: Option[LocalDate]
  )

  trait HasUserInfo[F[_]]:
    def get: F[UserInfo]

  def propToValue[F[_]: Applicative: HasUserInfo]: Option[UserProp] => F[Option[FieldValue]] =
    case None => Option.empty.pure[F]
    case Some(prop) =>
      summon[HasUserInfo[F]].get.map {
        case UserInfo(fst, lst, brth) =>
          prop match
            case UserProp.firstName => fst.map(FieldValue.string(_))
            case UserProp.lastName => lst.map(FieldValue.string(_))
            case UserProp.birthDate => brth.map(FieldValue.date(_))
      }

  def render[F[_]: Applicative: HasUserInfo: Localizer, A](
    lang: Lang
  ): Template[A] => F[Rendered[A]] =
    case Template.L(key) => summon[Localizer[F]].localize(lang, key).map(Rendered.L(_))
    case Template.C(prop) => propToValue[F].apply(prop).map(Rendered.C(_))

  def renderForm[F[_]: Applicative: HasUserInfo: Localizer](
    lang: Lang
  ): Form[Template] => F[Form[Rendered]] =
    summon[TraverseK[Form]].traverseK(_)([A] => (t: Template[A]) => render[F, A](lang).apply(t))

  override def run(args: List[String]): IO[ExitCode] =
    given Localizer[IO] = Localizer.make[IO]
    given HasUserInfo[IO] = new HasUserInfo[IO]:
      def get: IO[UserInfo] = IO.pure(UserInfo(Some("Владимир"), None, None))

    val nameformL =
      Form[Template](
        Template.L(LKey("name_form_title")),
        Field(Template.L(LKey("first_name_field_title")), Template.L(LKey("name_hint")), Template.C(Some(UserProp.firstName)), FieldType.string) ::
          Field(Template.L(LKey("last_name_field_title")), Template.L(LKey("name_hint")), Template.C(Some(UserProp.lastName)), FieldType.string) ::
          Field(Template.L(LKey("birth_date_field_title")), Template.L(LKey("birth_date_hint")), Template.C(Some(UserProp.birthDate)), FieldType.date) ::
          Nil,
        Action("send", Template.L(LKey("confirm_action_title")), ActionType.send) :: Nil
      )

    renderForm[IO](Lang.EN).apply(nameformL).flatMap(
      Console[IO].println(_)
    ).as(ExitCode.Success)
