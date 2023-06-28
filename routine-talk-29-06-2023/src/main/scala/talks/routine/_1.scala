package talks.routine

import io.circe.{Decoder, Encoder}
import io.circe.syntax.*
import io.circe.parser.parse

import java.time.LocalDate

// Только локализация, реализация без тайп мемберов в моделях и ньютайпов
object _1:
  case class Form(title: String, fields: List[Field], actions: List[Action])
    derives Encoder.AsObject

  case class Field(title: String, hint: String, `type`: FieldType)
    derives Encoder.AsObject

  case class Action(id: String, title: String, `type`: ActionType)
    derives Encoder.AsObject

  enum FieldType:
    case string, date

  object FieldType:
    given Encoder[FieldType] = Encoder[String].contramap(_.toString)

  enum ActionType:
    case send

  object ActionType:
    given Encoder[ActionType] = Encoder[String].contramap(_.toString)

  case class FormValue(
    fields: List[FieldValue],
    actionId: String
  ) derives Decoder

  enum FieldValue derives Decoder:
    case string(value: String)
    case date(value: LocalDate)

  enum Lang:
    case RU, EN

  val keys: Map[String, (String, String)] = Map(
    "name_form_title" -> ("Введите свои данные", "Enter personal info"),
    "first_name_field_title" -> ("Имя", "First name"),
    "last_name_field_title" -> ("Фамилия", "Last name"),
    "name_hint" -> ("Мой ответ", "My answer"),
    "birth_date_field_title" -> ("Дата рождения", "Date of birth"),
    "birth_date_hint" -> ("Дата", "Date"),
    "confirm_action_title" -> ("Подтвердить", "Confirm"),
  )

  def localize(lang: Lang): String => String = key =>
    keys.get(key).fold(key) { // не обрабатываем отсутствие перевода
      case (ru, en) =>
        lang match
          case Lang.RU => ru
          case Lang.EN => en
    }

  def localizeField(lang: Lang): Field => Field =
    case Field(title, hint, typ) =>
      Field(localize(lang)(title), localize(lang)(hint), typ)
      // Field(localize(lang)(title), hint, typ) тоже будет компилироваться

  def localizeAction(lang: Lang): Action => Action =
    case Action(id, title, typ) =>
      Action(id, localize(lang)(title), typ)

  def localizeForm(lang: Lang): Form => Form =
    case Form(title, fields, actions) =>
      Form(
        localize(lang)(title),
        fields.map(localizeField(lang)),
        actions.map(localizeAction(lang))
      )

  @main def r1: Unit =
    val nameform = Form(
      "Введите свои данные",
      Field("Имя", "Мой ответ", FieldType.string) ::
        Field("Фамилия", "Мой ответ", FieldType.string) ::
        Field("Дата рождения", "Дата", FieldType.date) ::
        Nil,
      Action("send", "Подтвердить", ActionType.send) :: Nil
    )

    println(nameform.asJson)

    val filled =
      """
        |{
        |  "fields": [
        |    { "string": { "value": "Владимир" } },
        |    { "string": { "value": "Ленский" } },
        |    { "date":   { "value": "1823-07-27" } }
        |  ],
        |  "actionId": "send"
        |}
        |""".stripMargin

    val nameformL =
      Form(
        "name_form_title",
        Field("first_name_field_title", "name_hint", FieldType.string) ::
          Field("last_name_field_title", "name_hint", FieldType.string) ::
          Field("birth_date_field_title", "birth_date_hint", FieldType.date) ::
          Nil,
        Action("send", "confirm_action_title", ActionType.send) :: Nil
      )

    println(parse(filled).flatMap(_.as[FormValue]))
    println(localizeForm(Lang.RU)(nameformL))
    println(localizeForm(Lang.EN)(nameformL))