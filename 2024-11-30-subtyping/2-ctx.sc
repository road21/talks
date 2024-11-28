//> using dep dev.zio::izumi-reflect:2.3.10
import izumi.reflect.Tag
import izumi.reflect.macrortti.LightTypeTag

trait Ctx[+R]:
  def add[X: Tag](x: X): Ctx[R & X]
  def get[X >: R: Tag]: X

object Ctx:
  private case class Impl[+R](map: Map[LightTypeTag, Any]) extends Ctx[R]:
    def add[X: Tag](x: X): Ctx[R & X] =
      Impl[R & X](map + (Tag[X].tag -> x))
    def get[X >: R: Tag]: X =
      map.getOrElse(Tag[X].tag, throw new Exception("shouldn't happen")).asInstanceOf[X]

  def empty: Ctx[Any] = new Impl[Any](Map())

// example
val ctx: Ctx[String & Boolean] =
  Ctx.empty
    .add[String]("it")
    .add[Boolean](true)

val str = ctx.get[String]
println(s"I got $str!") // prints "I got it!"

val bool = ctx.get[Boolean]
println(s"I got $bool!") // prints "I got true!"

// ctx.get[Int] // doesn't compile

// ctx.get[String & Boolean] // Caused by: java.lang.Exception: shouldn't happen