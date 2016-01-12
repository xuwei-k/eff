package org.specs2.control.eff

import cats.data._, Xor._
import Interpret._
import Eff._
import Effects._

/**
 * Effect for optional computations
 */
object OptionEffect {

  /** create an Option effect from a single Option value */
  def fromOption[R, A](option: Option[A])(implicit member: Member[Option[?], R]): Eff[R, A] =
    option.fold[Eff[R, A]](none)(some)

  /** no value returned */
  def none[R, A](implicit member: Member[Option[?], R]): Eff[R, A] =
    send[Option, R, A](None)

  /** a value is returned */
  def some[R, A](a: A)(implicit member: Member[Option[?], R]): Eff[R, A] =
    send[Option, R, A](Some(a))

  /**
   * Interpret the Option effect
   *
   * Stop all computations if None is present once
   */
  def runOption[R <: Effects, A](r: Eff[Option |: R, A]): Eff[R, Option[A]] = {
    val recurse = new Recurse[Option, R, Option[A]] {
      def apply[X](m: Option[X]) =
        m match {
          case None    => Right(EffMonad[R].pure(None))
          case Some(x) => Left(x)
        }
    }

    interpret1[R, Option, A, Option[A]]((a: A) => Option(a))(recurse)(r)
  }

  trait Removed[R <: Effects, U <: Effects, M[_]]


  def runOption1[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member[Option, R], poly: Removed[R, U, Option]): Eff[U, Option[A]] =
    r match {
      case Pure(a) => EffMonad[U].pure(Some(a))
      case Impure(u, c) =>
        m.project(u) match {
          case Some(Some(x)) =>
            runOption1(c(x))
          case Some(None) =>
            EffMonad[U].pure(None)
          case None =>
            u match {
              case UnionNext(n) =>
                Impure(n.asInstanceOf[Union[R, u.X]], c).asInstanceOf[Eff[U, Option[A]]]
              case UnionNow(mx) =>
                Impure[U, u.X, Option[A]](u.asInstanceOf[Union[U, u.X]], Arrs.singleton(x => runOption1(c(x)))).asInstanceOf[Eff[U, Option[A]]]
            }
        }
    }

  implicit def removedFirst[R <: Effects, M[_]](implicit m: Member[M, M |: R]): Removed[M |: R, R, M] =
    new Removed[M |: R, R, M] {}

  implicit def removedNext[R <: Effects, U <: Effects, N[_], M[_]](implicit m: Member[M, R], removed: Removed[R, U, M]): Removed[N |: R, N |: U, M] =
    new Removed[N |: R, N |: U, M] {}

}

