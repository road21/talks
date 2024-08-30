//> using dep org.scalacheck::scalacheck:1.18.0
//> using dep org.scalameta::munit-scalacheck:1.0.0

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import org.scalacheck.Prop.*
import scala.math.Ordering
import scala.collection.SortedSet
import scala.math.Ordering.Implicits.infixOrderingOps

case class User(
  // ...
  name: String,
  age: Option[Int]
  // ...
)

def top3Users(users: Vector[User]): Vector[User] =
  top3(users)

def top3[A: Ordering](elems: Vector[A]): Vector[A] =
  if (elems.size > 3)
    val (first3, tail) = elems.splitAt(3)
    tail.foldLeft(first3.sorted) {
      case (acc, u) =>
        if (u <= acc(0)) acc
        else (acc.tail :+ u).sorted
      }.toVector
  else elems.sorted

given Ordering[User] with
  def compare(x: User, y: User): Int =
    (x, y) match
      case (User(_, Some(xAge)), User(_, Some(yAge))) =>
        summon[Ordering[Int]].compare(xAge, yAge)
      //case (User(_, Some(x)), _) => 1
      //case (_, User(_, Some(x))) => -1
      case _ =>
        summon[Ordering[String]].compare(x.name, y.name)

given Arbitrary[User] =
  Arbitrary(
    for
      name <- Gen.alphaStr
      age <- Gen.oneOf(Gen.const(None), Gen.choose(0, 200).map(Some(_)))
    yield User(name, age)
  )

extension [A](vect: Vector[A])
  def isSorted(using ord: Ordering[A]): Boolean =
    vect match
      case h +: t =>
        t.foldLeft((true, h)) {
          case ((res, curr), el) =>
            (res && (curr <= el), el)
        }._1
      case _ => true

class UserTop3Spec extends munit.FunSuite, munit.ScalaCheckSuite:
  test("top3 works"):
    val turing = User("Alan", Some(41))
    val hughes = User("John", None)
    val curry = User("Haskell", Some(81))
    val maclane = User("Saunders", Some(95))
    val yoneda = User("Nobuo", Some(66))
    val pierce = User("Benjamin", None)

    assertEquals(
      top3(Vector(maclane, yoneda, curry, hughes, turing, pierce)),
      Vector(yoneda, curry, maclane)
    )

  property("top3 result is sorted and all other elements are less or equals than first element of result"):
    forAll { (users: Vector[User]) =>
      val top = top3Users(users)
      val checkSize = if (users.size >= 3) top.size == 3
                      else top.size == users.size
      checkSize && { top match
        case h +: t => users.filterNot(top.contains).forall(_ <= h) && top.isSorted
        case _      => true
      }
    }


class UserSpec extends munit.ScalaCheckSuite:
  property("user ordering should be reflexive"):
    forAll { (a: User) => (a <= a) == true }

  // property("user ordering should be anti-symmetric"):
  //   forAll { (a: User, b: User) =>
  //     (a <= b && b <= a) ==> (a equiv b)
  //   }

  property("user ordering should be transitive"):
    forAll { (a: User, b: User, c: User) =>
      (a <= b && b <= c) ==> a <= c
    }
