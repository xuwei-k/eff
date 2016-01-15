package org.specs2.control.eff

import cats.arrow.NaturalTransformation
import cats.data._
import Effects._
import Tag._
import cats.data._, Xor._

/**
 * Open union of effects
 *
 * They are modelled as a list of
 *
 *  UnionNext(UnionNext(...(UnionNow(M[X])))
 *
 * where M[X] is an effect. The depth of the nesting in an Union value
 * corresponds to the place of the effect in a type E1 |: E2 |: E3 |: .. |: NoEffect
 *
 */
trait Union[R, A] {
  type X = A
}

case class UnionNow[T[_], R <: Effects, A](ta: T[A]) extends Union[T |: R, A]

case class UnionNext[O[_], R <: Effects, A](u: Union[R, A]) extends Union[O |: R, A]

/**
 * create union objects
 */
object Union {
  def now[T[_], R <: Effects, A](ta: T[A]): Union[T |: R, A] =
    UnionNow(ta)

  def next[O[_], R <: Effects, A](u: Union[R, A]): Union[O |: R, A] =
    UnionNext(u)

  /**
   * decompose a union starting with a given effect into
   *
   *  - a value for that effect type if there is one
   *  - the union with the remaining effects
   */
  def decompose[T[_], R <: Effects, V](u: Union[T |: R, V]): Union[R, V] Xor T[V] =
    u match {
      case UnionNow(tv)     => Right(tv)
      case UnionNext(union) => Left(union)
    }

}

/**
 * Member typeclass for effects belonging to a stack of effects R
 *
 * If T is a member of R then we can:
 *
 * - create a Union of effects from a single effect with "inject"
 * - extract an effect value from a union if there is such an effect in the stack
 */
trait Member[T[_], R] {
  type Out <: Effects

  def inject[V](tv: T[V]): Union[R, V]

  def project[V](u: Union[R, V]): Option[T[V]]
}

object Member extends MemberImplicits {

  type Aux[T[_], R, U] = Member[T, R] { type Out = U }

  def ZeroMember[T[_], R <: Effects]: Member.Aux[T, T |: R, R] = new Member[T, T |: R] {
    type Out = R

    def inject[V](effect: T[V]): Union[T |: R, V] =
      Union.now(effect)

    def project[V](union: Union[T |: R, V]): Option[T[V]] =
      union match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }

  def SuccessorMember[T[_], O[_], R <: Effects, U <: Effects](implicit o: Member.Aux[O, O |: R, R], m: Member.Aux[T, R, U]): Member.Aux[T, O |: R, O |: U] = new Member[T, O |: R] {
    type Out = O |: U

    def inject[V](effect: T[V]) =
      Union.next(m.inject[V](effect))

    def project[V](union: Union[O |: R, V]) =
      union match {
        case UnionNow(_) => None
        case UnionNext(u) => m.project[V](u)
      }
  }

  /**
   * helper method to untag a tagged effect
   */
  def untagMember[T[_], R, TT](m: Member[({type X[A]=T[A] @@ TT})#X, R]): Member.Aux[T, R, m.Out] =
    new Member[T, R] {
      type Out = m.Out

      def inject[V](tv: T[V]): Union[R, V] =
        m.inject(Tag(tv))

      def project[V](u: Union[R, V]): Option[T[V]] =
        m.project(u).map(Tag.unwrap)
    }

  type <=[M[_], R] = Member[M, R]
}

trait MemberImplicits {
  implicit def ZeroMemberInfer[T[_], R <: Effects]: Member.Aux[T, T |: R, R] =
    Member.ZeroMember[T, R]

  implicit def SuccessorMemberInfer[T[_], O[_], R <: Effects, U <: Effects](implicit o: Member.Aux[O, O |: R, R], m: Member.Aux[T, R, U]): Member.Aux[T, O |: R, O |: U] =
    Member.SuccessorMember[T, O, R, U](o, m)
}
