package org.specs2.control.eff


import cats.data._, Xor._
import cats.syntax.functor._
import Eff._
import Interpret._
import Effects.|:

/**
 * Effect for computation which can fail
 */
object DisjunctionEffect {

  /** create a Disjunction effect from a single Option value */
  def fromOption[R, E, A](option: Option[A], e: E)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    option.fold[Eff[R, A]](left[R, E, A](e))(right[R, E, A])

  /** create a Disjunction effect from a single Xor value */
  def fromXor[R, E, A](xor: E Xor A)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    xor.fold[Eff[R, A]](left[R, E, A], right[R, E, A])

  /** create a failed value */
  def left[R, E, A](e: E)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    send[E Xor ?, R, A](Left(e))

  /** create a correct value */
  def right[R, E, A](a: A)(implicit member: Member[(E Xor ?), R]): Eff[R, A] =
    send[E Xor ?, R, A](Right(a))

  /** run the disjunction effect, yielding E Xor A */
  def runDisjunction[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, E Xor A] = {
    val recurse = new Recurse[(E Xor ?), U, E Xor A] {
      def apply[X](m: E Xor X) =
        m match {
          case Left(e) => Right(EffMonad[U].pure(Left(e)))
          case Right(a) => Left(a)
        }
    }

    interpret1[R, U, (E Xor ?), A, E Xor A]((a: A) => Right(a): E Xor A)(recurse)(r)
  }

  /** run the disjunction effect, yielding Either[E, A] */
  def runDisjunctionEither[R <: Effects, U <: Effects, E, A](r: Eff[R, A])(implicit m: Member.Aux[(E Xor ?), R, U]): Eff[U, Either[E, A]] =
    runDisjunction(r).map(_.fold(util.Left.apply, util.Right.apply))

  implicit def DisjunctionMemberInfer[E, R <: Effects]: Member.Aux[Xor[E, ?], Xor[E, ?] |: R, R] =
    Member.ZeroMember[Xor[E, ?], R]

}
