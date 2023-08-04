package dh.lang.data

trait Bound[A]:
  def apply(a: A): Either[String, Unit]

trait Refined[A]:
  opaque type T <: A = A
  def bounds: Vector[Bound[A]] = Vector.empty

  def apply(a: A): Either[String, T] =
    val (errors, _) = bounds.partitionMap(_.apply(a))
    if (errors.isEmpty) Right(a)
    else Left("Errors: " + errors.mkString(","))

case class RegexB(pattern: String) extends Bound[String]:
  def apply(a: String): Either[String, Unit] =
    Either.cond(a.matches(pattern), (), s"string $a doesn't match pattern $pattern")

trait Like(pattern: String) extends Refined[String]:
  override def bounds: Vector[Bound[String]] = super.bounds.prepended(RegexB(pattern))

trait StringNewType:
  opaque type T <: String = String
  def apply(a: String): T = a
