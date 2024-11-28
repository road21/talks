import scala.annotation.tailrec
import scala.compiletime.summonFrom

object Role:
  type email = "email"
  val email: email = "email"

  type phone = "phone"
  val phone: phone = "phone"

  type device_id = "device_id"
  val device_id: device_id = "device_id"

case class User[+Roles] private (
                                  roles: Set[String],
                                  emailOpt: Option[String],
                                  emailVerifiedOpt: Option[Boolean],
                                  phoneOpt: Option[String],
                                  phoneVerifiedOpt: Option[Boolean],
                                  deviceIdOpt: Option[String]
                                ) {
  def email(using Roles <:< Role.email): String = emailOpt.get // get is safe here
  def emailVerified(using Roles <:< Role.email): Boolean = emailVerifiedOpt.get
  def phone(using Roles <:< Role.phone): String = phoneOpt.get
  def phoneVerified(using Roles <:< Role.phone): Boolean = phoneVerifiedOpt.get
  def deviceId(using Roles <:< Role.device_id): String = deviceIdOpt.get

  def setEmail(email: String, emailVerified: Boolean): User[Roles & Role.email] =
    User[Roles & Role.email](
      roles + Role.email,
      Some(email),
      Some(emailVerified),
      phoneOpt,
      phoneVerifiedOpt,
      deviceIdOpt
    )

  def setPhone(phone: String, phoneVerified: Boolean): User[Roles & Role.phone] =
    User[Roles & Role.phone](
      roles + Role.phone,
      emailOpt,
      emailVerifiedOpt,
      Some(phone),
      Some(phoneVerified),
      deviceIdOpt
    )

  def setDeviceId(deviceId: String): User[Roles & Role.phone] =
    User[Roles & Role.phone](
      roles + Role.device_id,
      emailOpt,
      emailVerifiedOpt,
      phoneOpt,
      phoneVerifiedOpt,
      Some(deviceId)
    )

  inline def checkRoles[S]: Either[Set[String], User[Roles & S]] =
    val checkEmail = summonFrom {
      case _: (S <:< Role.email) => if (roles.contains(Role.email)) Set() else Set(Role.email)
      case _ => Set()
    }

    val checkPhone = summonFrom {
      case _: (S <:< Role.phone) => if (roles.contains(Role.phone)) Set() else Set(Role.phone)
      case _ => Set()
    }

    val checkDeviceId = summonFrom {
      case _: (S <:< Role.device_id) => if (roles.contains(Role.device_id)) Set() else Set(Role.device_id)
      case _ => Set()
    }

    val needRoles = checkEmail ++ checkPhone ++ checkDeviceId
    Either.cond(needRoles.isEmpty, this.asInstanceOf[User[Roles & S]], needRoles)
}

object User:
  def empty: User[Any] =
    User(Set(), None, None, None, None, None)

case class UserRaw(
                    roles: List[String],
                    emailOpt: Option[String],
                    emailVerifiedOpt: Option[Boolean],
                    phoneOpt: Option[String],
                    phoneVerifiedOpt: Option[Boolean],
                    deviceIdOpt: Option[String]
                  )

// validations
import scala.compiletime.*

object UserValidation:
  private def validateStep(raw: UserRaw, roles: List[String], acc: User[?]): Either[String, (List[String], User[?])] =
    def fmtErr(field: String, role: String): String =
      s"Field $field is not set, but user has $role role"

    roles match
      case h :: t =>
        h match
          case Role.email =>
            for
              email <- raw.emailOpt.toRight(fmtErr("email", Role.email))
              emailVerified <- raw.emailVerifiedOpt.toRight(fmtErr("emailVerified", Role.email))
            yield (t, acc.setEmail(email, emailVerified))
          case Role.phone =>
            for
              phone <- raw.phoneOpt.toRight(fmtErr("phone", Role.phone))
              phoneVerified <- raw.phoneVerifiedOpt.toRight(fmtErr("phoneVerified", Role.phone))
            yield (t, acc.setPhone(phone, phoneVerified))
          case Role.device_id =>
            raw.deviceIdOpt.toRight(fmtErr("deviceId", Role.device_id)).map(
              deviceId => t -> acc.setDeviceId(deviceId)
            )
      case Nil => Right((Nil, acc))

  @tailrec
  private def validateImpl(raw: UserRaw, rolesAcc: List[String], userAcc: User[?]): Either[String, User[?]] =
    validateStep(raw, rolesAcc, userAcc) match
      case Left(value) => Left(value)
      case Right((Nil, user)) => Right(user)
      case Right((roles, user)) => validateImpl(raw, roles, user)

  def validateSome(raw: UserRaw): Either[String, User[?]] =
    validateImpl(raw, raw.roles, User.empty)

  inline def validate[S](raw: UserRaw): Either[String, User[S]] =
    validateSome(raw).flatMap(
      user => user.checkRoles[S].left.map(roles => s"User doens't have roles: $roles")
    )

// examples

import UserValidation.*

val userRaw: UserRaw =  UserRaw("email" :: Nil, Some("lol@email.ru"), Some(true), None, None, None)
validate[Role.email](userRaw)
validate[Role.phone](userRaw)
validate[Role.email & Role.phone & Role.device_id](userRaw)

val withPhone: Either[String, User[Role.email & Role.phone]] =
  validate[Role.email](userRaw).map(_.setPhone("+7", false))

val userRaw2: UserRaw =  UserRaw("email" :: "phone" :: Nil, Some("lol@email.ru"), Some(true), Some("+7"), Some(false), None)
validate[Role.phone & Role.email](userRaw2)
