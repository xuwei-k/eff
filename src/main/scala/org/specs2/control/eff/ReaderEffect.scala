package org.specs2.control.eff

import cats._, data._, Xor._
import Interpret._
import Tag._
import Eff._
import Effects.|:

/**
 * Effect for computations depending on an environment.
 *
 * The inside datatype for this effect is scalaz.Reader
 *
 * Several Reader effects can be present in a given stack provided that they are tagged with scala.Tag.
 *
 * A tagged Reader effect can be run with runTaggedReader
 *
 */
object ReaderEffect {

  /** get the environment */
  def ask[R, T](implicit member: Member[Reader[T, ?], R]): Eff[R, T] =
    local[R, T, T](identity)

  /** get the environment */
  def askTagged[R, Tg, T](implicit member: Member[({type l[X] = Reader[T, X] @@ Tg})#l, R]): Eff[R, T] =
    localTagged[R, Tg, T, T](identity)

  /** modify the environment */
  def local[R, T, U](f: T => U)(implicit member: Member[Reader[T, ?], R]): Eff[R, U] =
    send[Reader[T, ?], R, U](Reader(f))

  /** modify the environment */
  def localTagged[R, Tg, T, U](f: T => U)(implicit member: Member[({type l[X] = Reader[T, X] @@ Tg})#l, R]): Eff[R, U] =
    send[({type l[X] = Reader[T, X] @@ Tg})#l, R, U](Tag(Reader(f)))

  /** interpret the Reader effect by providing an environment when required */
  def runReader[R <: Effects, U <: Effects, A, B](env: A)(r: Eff[R, B])(implicit m: Member.Aux[Reader[A, ?], R, U]): Eff[U, B] = {
    val recurse = new Recurse[Reader[A, ?], U, B] {
      def apply[X](m: Reader[A, X]) = left(m.run(env))
    }

    interpret1[R, U, Reader[A, ?], B, B]((b: B) => b)(recurse)(r)
  }

  /** interpret a tagged Reader effect by providing an environment when required */
  def runTaggedReader[R <: Effects, U <: Effects, T, A, B](env: A)(r: Eff[R, B])(implicit
         m: Member.Aux[({type l[X] = Reader[A, X] @@ T})#l, R, U]): Eff[U, B] = {

    val recurse = new Recurse[({type l[X] = Reader[A, X] @@ T})#l, U, B] {
      def apply[X](m: Reader[A, X] @@ T) = Left(Tag.unwrap(m).run(env))
    }

    interpret1[R, U, ({type l[X] = Reader[A, X] @@ T})#l, B, B]((b: B) => b)(recurse)(r)
  }
}

trait ReaderImplicits {
  implicit def ReaderMemberInfer[A, R <: Effects]: Member.Aux[Reader[A, ?], Reader[A, ?] |: R, R] =
    Member.ZeroMember[Reader[A, ?], R]
}

object ReaderImplicits extends ReaderImplicits
