package talks.routine

import scala.deriving.Mirror
import io.circe.parser.parse
import io.circe.{Decoder, Encoder}

// Только локализация, реализация без тайп мемберов в моделях с ньютайпами
object _2:
  type Text = Text.T
  object Text extends TextInstances:
    opaque type T <: String = String
    def apply(key: String): T = key

  trait TextInstances:
    given Encoder[Text] = Encoder[String].contramap(x => x)

  type LKey = LKey.K
  object LKey extends LKeyInstances:
    opaque type K <: String = String
    def apply(key: String): K = key

  trait LKeyInstances:
    given Encoder[LKey] = Encoder[String].contramap(x => x)

  extension (e: Encoder.type)
    inline def derived[A](using inline A: Mirror.Of[A]): Encoder[A] =
      Encoder.AsObject.derived[A]

  case class FormL(title: LKey, fields: List[FieldL], actions: List[ActionL])
    derives Encoder

  case class FieldL(title: LKey, hint: LKey, `type`: FieldType)
    derives Encoder

  case class ActionL(id: String, title: LKey, `type`: ActionType)
    derives Encoder

  case class Form(title: Text, fields: List[Field], actions: List[Action])
    derives Encoder

  case class Field(title: Text, hint: Text, `type`: FieldType)
    derives Encoder

  case class Action(id: String, title: Text, `type`: ActionType)
    derives Encoder

  enum FieldType:
    case string, date

  object FieldType:
    given Encoder[FieldType] = Encoder[String].contramap(_.toString)

  enum ActionType:
    case send

  object ActionType:
    given Encoder[ActionType] = Encoder[String].contramap(_.toString)

  enum Lang:
    case RU, EN

  val keys: Map[LKey, (Text, Text)] = Map(
    "name_form_title" -> ("Введите свои данные", "Enter personal info"),
    "first_name_field_title" -> ("Имя", "First name"),
    "last_name_field_title" -> ("Фамилия", "Last name"),
    "name_hint" -> ("Мой ответ", "My answer"),
    "birth_date_field_title" -> ("Дата рождения", "Date of birth"),
    "birth_date_hint" -> ("Дата", "Date"),
    "confirm_action_title" -> ("Подтвердить", "Confirm"),
  ).map { case (k, (en, ru)) => LKey(k) -> (Text(en), Text(ru)) }

  def localize(lang: Lang): LKey => Text = key =>
    keys.get(key).fold(Text(key)) { // не обрабатываем отсутствие перевода
      case (ru, en) =>
        lang match
          case Lang.RU => ru
          case Lang.EN => en
    }

  def localizeField(lang: Lang): FieldL => Field =
    case FieldL(title, hint, typ) =>
      Field(localize(lang)(title), localize(lang)(hint), typ)
  // Field(localize(lang)(title), hint, typ) тоже будет компилироваться

  def localizeAction(lang: Lang): ActionL => Action =
    case ActionL(id, title, typ) =>
      Action(id, localize(lang)(title), typ)

  def localizeForm(lang: Lang): FormL => Form =
    case FormL(title, fields, actions) =>
      Form(
        localize(lang)(title),
        fields.map(localizeField(lang)),
        actions.map(localizeAction(lang))
      )

  @main def r2: Unit =
    val nameformL =
      FormL(
        LKey("name_form_title"),
        FieldL(LKey("first_name_field_title"), LKey("name_hint"), FieldType.string) ::
          FieldL(LKey("last_name_field_title"), LKey("name_hint"), FieldType.string) ::
          FieldL(LKey("birth_date_field_title"), LKey("birth_date_hint"), FieldType.date) ::
          Nil,
        ActionL("send", LKey("confirm_action_title"), ActionType.send) :: Nil
      )

    println(localizeForm(Lang.RU)(nameformL))
    println(localizeForm(Lang.EN)(nameformL))
