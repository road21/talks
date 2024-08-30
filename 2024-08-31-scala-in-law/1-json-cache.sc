import java.util.UUID

enum Json:
  case JNull
  case JBoolean(value: Boolean)
  case JNumber(value: Long)
  case JString(value: String)
  case JArray(value: Vector[Json])
  case JObject(value: Map[String, Json])

/**
 *  forall { (cache: JsonCache, json: Json) =>
 *    { cache.put(x, json) ; cache.read(x) } = json
 *  }
 **/
class JsonCache:
  import scala.collection.concurrent.{TrieMap, Map}
  val map: Map[UUID, Json] = TrieMap()

  def get(k: UUID): Option[Json] =
    map.get(k)

  def put(k: UUID, json: Json): Unit =
    map.put(k, json)

//-----------------------------------------------------------------------------

case class User(id: UUID, name: String)

enum OfferType:
  case Default
  case Custom

case class Offer(id: UUID, typ: OfferType)

/**
  * Требуется научиться реализовывать кэши для различных моделей
  *   (UserCache, OfferCache, ...), так, что (на примере User):
  *
  *   forall { (cache: UserCache, u: User) =>
  *     { cache.put(u) ; cache.get(u.id) } == u
  *   }
  * */

//-----------------------------------------------------------------------------

import Json.*
import scala.util.Try

object Step1 {
  def writeStr(str: String): Json = JString(str)
  def readStr(json: Json): Option[String] =
    Some(json).collect { case JString(str) => str }

  def writeUUID(uuid: UUID): Json = JString(uuid.toString)
  def readUUID(json: Json): Option[UUID] =
    Some(json).collect { case JString(str) => str }
      .flatMap(res => Try(UUID.fromString(res)).toOption)

  def writeOfferType(typ: OfferType): Json =
    typ match
      case OfferType.Default => writeStr("default")
      case OfferType.Custom => writeStr("custom")

  def readOfferType(json: Json): Option[OfferType] =
    readStr(json).collect {
      case "default" => OfferType.Default
      case "custom"  => OfferType.Custom
    }

  def readUser(json: Json): Option[User] =
    for
      obj <- Some(json).collect { case JObject(obj) => obj }
      id  <- obj.get("id").flatMap(readUUID)
      name <- obj.get("name").flatMap(readStr)
    yield User(id, name)

  def writeUser(user: User): Json =
    JObject(Map(
      "id"     -> writeUUID(user.id),
      "name"   -> writeStr(user.name)
    ))

  def readOffer(json: Json): Option[Offer] =
    for
      obj <- Some(json).collect { case JObject(obj) => obj }
      id  <- obj.get("id").flatMap(readUUID)
      typ <- obj.get("typ").flatMap(readOfferType)
    yield Offer(id, typ)

  def writeOffer(offer: Offer): Json =
    JObject(Map(
      "id"  -> writeUUID(offer.id),
      "typ" -> writeOfferType(offer.typ)
    ))

  class EntityCache[A](
    write: A => Json,        // для выполнения свойств необходимо требовать консистентность write/read
    read: Json => Option[A],
    key: A => UUID,
    cache: JsonCache
  ):
    def get(id: UUID): Option[A] =
      cache.get(id).flatMap(read)

    def put(a: A): Unit =
      cache.put(key(a), write(a))

  class UserCache(cache: JsonCache)
    extends EntityCache(writeUser, readUser, _.id, cache)

  class  OfferCache(cache: JsonCache)
    extends EntityCache(writeOffer, readOffer, _.id, cache)
}

trait JsonCodec[A]:
  /**
    * forall { (a: A) =>
    *   write(a).flatMap(read) == Some(a)
    * }
    */
  def write(a: A): Json
  def read(json: Json): Option[A]

object JsonCodec:
  inline def apply[A](using j: JsonCodec[A]): JsonCodec[A] = j

/**
  *  forall A: Type, cache: EntityCache[A], a: A.
  *    { cache.put(a) ; cache.get(cache.key(a)) } = u
  */
case class EntityCache[A](key: A => UUID, cache: JsonCache)(using codec: JsonCodec[A]):
  def get(id: UUID): Option[A] =
    cache.get(id).flatMap(codec.read)

  def put(a: A): Unit =
    cache.put(key(a), codec.write(a))

given JsonCodec[String] with
  def write(str: String): Json = JString(str)
  def read(json: Json): Option[String] =
    Some(json).collect { case JString(str) => str }

given JsonCodec[UUID] with
  def write(uuid: UUID): Json = JString(uuid.toString)
  def read(json: Json): Option[UUID] =
    Some(json).collect { case JString(str) => str }
      .flatMap(res => Try(UUID.fromString(res)).toOption)

given JsonCodec[OfferType] with
  def write(typ: OfferType): Json =
    JsonCodec[String].write {
      typ match
        case OfferType.Default => "default"
        case OfferType.Custom => "custom"
    }

  def read(json: Json): Option[OfferType] =
    JsonCodec[String].read(json).collect {
      case "default" => OfferType.Default
      case "custom"  => OfferType.Custom
    }

given JsonCodec[User] with
  def read(json: Json): Option[User] =
    for
      obj <- Some(json).collect { case JObject(obj) => obj }
      id  <- obj.get("id").flatMap(JsonCodec[UUID].read)
      name <- obj.get("name").flatMap(JsonCodec[String].read)
    yield User(id, name)

  def write(user: User): Json =
    JObject(Map(
      "id"     -> JsonCodec[UUID].write(user.id),
      "name"   -> JsonCodec[String].write(user.name)
    ))

given JsonCodec[Offer] with
  def read(json: Json): Option[Offer] =
    for
      obj <- Some(json).collect { case JObject(obj) => obj }
      id  <- obj.get("id").flatMap(JsonCodec[UUID].read)
      typ <- obj.get("typ").flatMap(JsonCodec[OfferType].read)
    yield Offer(id, typ)

  def write(offer: Offer): Json =
    JObject(Map(
      "id"  -> JsonCodec[UUID].write(offer.id),
      "typ" -> JsonCodec[OfferType].write(offer.typ)
    ))


val userCache2 = EntityCache[User](_.id, new JsonCache)
val uuid2 = UUID.fromString("9a60e5d0-77c2-4db5-b6f6-a3d686b76160")
val user2 = User(uuid2, "Bob")
userCache2.put(user2)
println(userCache2.get(uuid2))
