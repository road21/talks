//> using dep org.typelevel::cats-core:2.12.0

import scala.util.control.TailCalls
import cats.syntax.*
import cats.implicits.*

enum Tree[A]:
  case Node(ts: Vector[Tree[A]])
  case Leaf(v: A)

def map1[A, B](tree: Tree[A], f: A => B): Tree[B] =
  tree match
    case Tree.Node(ts) =>
      Tree.Node(
        ts.map(map1(_, f))
      )
    case Tree.Leaf(v)  => Tree.Leaf(f(v))

def map2[A, B](tree: Tree[A], f: A => B): Tree[B] =
  def run(in: Tree[A]): TailCalls.TailRec[Tree[B]] = in match
    case Tree.Node(ts) =>
      ts.traverse(t => TailCalls.tailcall(run(t))).map(Tree.Node(_))
    case Tree.Leaf(v)  =>
      TailCalls.done(Tree.Leaf(f(v)))

  run(tree).result


def bigTree(n: Int, acc: Tree[Int]): Tree[Int] =
  if (n == 0) acc
  else bigTree(n - 1, Tree.Node(Vector(acc)))


println("generating tree")
val tree = bigTree(1000000, Tree.Leaf(1))
println("mapping tree")
// map1(tree, _ + 1) // stackoverflow
map2(tree, _ + 1)
