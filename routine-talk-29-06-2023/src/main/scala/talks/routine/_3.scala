package talks.routine

import cats.{Applicative, Traverse}
import cats.derived.*
import io.circe.Encoder
import cats.syntax.applicative.*

import scala.deriving.Mirror
import cats.MonadThrow
import cats.effect.std.Console
import cats.effect.{ExitCode, IO, IOApp}

// Только локализация (с эффектом), реализация c тайпмембером
object _3 extends IOApp:
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

  implicit class Stx(e: Encoder.type):
    inline def derived[A](using inline A: Mirror.Of[A]): Encoder[A] =
      Encoder.AsObject.derived[A]

  enum FieldType:
    case string, date

  object FieldType:
    given Encoder[FieldType] = Encoder[String].contramap(_.toString)

  enum ActionType:
    case send

  object ActionType:
    given Encoder[ActionType] = Encoder[String].contramap(_.toString)

  case class Field[A](
    title: A,
    hint: A,
    typ: FieldType
  ) derives Traverse, Encoder

  case class Action[A](
    id: String,
    title: A,
    typ: ActionType
  ) derives Traverse, Encoder

  case class Form[A](
    title: A,
    fields: List[Field[A]],
    actions: List[Action[A]]
  ) derives Traverse, Encoder

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

    def make[F[_]: MonadThrow]: Localizer[F] = (lang: Lang, key: LKey) =>
      keys.get(LKey(key)).fold {
        MonadThrow[F].raiseError(new Exception(s"no translation for key $key"))
      } { case (ru, en) =>
        Text(
          lang match
            case Lang.RU => ru
            case Lang.EN => en
        ).pure[F]
      }

  def localizeForm[F[_]: Applicative](lang: Lang)(using loc: Localizer[F]): Form[LKey] => F[Form[Text]] =
    Traverse[Form].traverse(_)(loc.localize(lang, _))

  override def run(args: List[String]): IO[ExitCode] =
    given Localizer[IO] = Localizer.make[IO]

    val nameformL =
      Form(
        LKey("name_form_title"),
        Field(LKey("first_name_field_title"), LKey("name_hint"), FieldType.string) ::
          Field(LKey("last_name_field_title"), LKey("name_hint"), FieldType.string) ::
          Field(LKey("birth_date_field_title"), LKey("birth_date_hint"), FieldType.date) ::
          Nil,
        Action("send", LKey("confirm_action_title"), ActionType.send) :: Nil
      )

    localizeForm[IO](Lang.EN).apply(nameformL).flatMap(
      Console[IO].println(_)
    ).as(ExitCode.Success)
