package dh.lang

import cats.{Monad, Applicative}
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.flatMap.*
import dh.lang.core.LType
import dh.lang.data.{Identifier, RefId, RefName, RefNs}
import linker.CalcTyped
import LTree.{Ident, InfixOp}

trait Refer[F[_]]:
  def ref(set: Option[RefNs], name: RefName): F[Option[CalcTyped]]

object Refer:
  @inline def apply[F[_]](using r: Refer[F]): Refer[F] = r

object linker:
  case class Result(term: LTree, depends: Set[RefId])
  case class CalcTyped(id: RefId, typ: LType)

  def apply[F[_]: Monad: Refer](term: LTree): F[Result] = link(term)

  private def deref[F[_]: Refer: Monad](nsName: Option[RefNs], name: RefName): F[Option[Result]] =
    Refer[F]
      .ref(nsName, name)
      .map(_.map(t => Result(LTree.Calculation(t.id, t.typ), Set(t.id))))

  private def link[F[_]: Monad: Refer](term: LTree): F[Result] =
    term match {
      case Ident(name) =>
        RefName(name).toOption
          .flatTraverse(mName => deref[F](None, mName))
          .map(_.getOrElse(Result(term, Set())))

      case LTree.Select(select @ LTree.Select(Ident(nsName), mName), rName) =>
        (RefNs(nsName).toOption, RefName(s"$mName.$rName").toOption)
          .traverseN[F, Option[Result]] { case (sName, mName) =>
            deref[F](Some(sName), mName)
          }
          .map(_.flatten)
          .flatMap(
            _.fold(
              link(select).map(r => Result(LTree.Select(r.term, rName), r.depends))
            )(Applicative[F].pure)
          )

      case LTree.Select(ident @ Ident(fstName), sndName) =>
        val tryWithNs = (RefNs(fstName).toOption, RefName(sndName).toOption)
          .traverseN[F, Option[Result]] { case (sName, mName) =>
            deref[F](Some(sName), mName)
          }
          .map(_.flatten)

        RefName(s"$fstName.$sndName").toOption
          .flatTraverse(name => deref[F](None, name))
          .flatMap {
            case None => tryWithNs
            case x => x.pure[F]
          }
          .flatMap(
            _.fold(
              link(ident).map(r => Result(LTree.Select(r.term, sndName), r.depends))
            )(Applicative[F].pure)
          )

      case LTree.Select(term, method) =>
        link(term).map(r => Result(LTree.Select(r.term, method), r.depends))

      case LTree.Apply(term, args) =>
        (link(term), args.traverse(link)).mapN { case (tres, argsRes) =>
          val (tArgs, deps) = argsRes.foldLeft((Vector[LTree](), Set[RefId]())) {
            case ((args, ids), Result(arg, depends)) =>
              (args :+ arg, ids ++ depends)
          }
          Result(LTree.Apply(tres.term, tArgs), tres.depends ++ deps)
        }
      case LTree.InfixOp(left, op, right) =>
        (link(left), link(right)).mapN { case (Result(lTerm, lDeps), Result(rTerm, rDeps)) =>
          Result(InfixOp(lTerm, op, rTerm), lDeps ++ rDeps)
        }
      case x => Result(x, Set()).pure[F]
    }
