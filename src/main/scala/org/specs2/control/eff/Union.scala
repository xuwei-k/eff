package org.specs2.control.eff

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

object Member extends Lower {

  type Aux[T[_], R, U] = Member[T, R] { type Out = U }

  /**
   * Implicits for determining if an effect T is member of a stack R
   *
   * Member uses MemberNat which tracks the "depth" of the effect in the stack R
   * using type-level naturals
   */
//  implicit def infer[T[_], R <: Effects, U <: Effects](implicit m: MemberNat.Aux[T, R, U]): Member.Aux[T, R, U] =
//    MemberNatIsMember[T, R, U](m)
//
//  def MemberNatIsMember[T[_], R <: Effects, U <: Effects](implicit m: MemberNat.Aux[T, R, U]): Member.Aux[T, R, U] = new Member[T, R] {
//    type Out = U
//
//    def inject[V](tv: T[V]): Union[R, V] =
//      m.inject(tv)
//
//    def project[V](u: Union[R, V]): Option[T[V]] =
//      m.project(u)
//  }
//  implicit def ZeroMember2[T[_,_], A]: Member.Aux[T[A,?], T[A,?] |: NoEffect, NoEffect] = new Member[T[A,?], T[A,?] |: NoEffect] {
//   type Out = NoEffect
//type TA[V] = T[A, V]
//    def inject[V](effect: TA[V]): Union[TA |: NoEffect, V] =
//      Union.now(effect)
//
//    def project[V](union: Union[TA |: NoEffect, V]): Option[TA[V]] =
//      union match {
//        case UnionNow(x) => Some(x)
//        case _ => None
//      }
//  }


  implicit def ZeroMember[T[_]]: Member.Aux[T, T |: NoEffect, NoEffect] = new Member[T, T |: NoEffect] {
   type Out = NoEffect

    def inject[V](effect: T[V]): Union[T |: NoEffect, V] =
      Union.now(effect)

    def project[V](union: Union[T |: NoEffect, V]): Option[T[V]] =
      union match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }
}
trait Lower {
  implicit def ZeroMemberR[T[_], R <: Effects]: Member.Aux[T, T |: R, R] = new Member[T, T |: R] {
   type Out = R

    def inject[V](effect: T[V]): Union[T |: R, V] =
      Union.now(effect)

    def project[V](union: Union[T |: R, V]): Option[T[V]] =
      union match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }

  implicit def SuccessorMember[T[_], O[_], R <: Effects](implicit o: Member[O, O |: R], m: Member[T, R]): Member.Aux[T, O |: R, O |: m.Out] = new Member[T, O |: R] {
    type Out = O |: m.Out

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
  def untagMember[T[_], R, TT](m: Member[({type X[A]=T[A] @@ TT})#X, R]): Member[T, R] =
    new Member[T, R] {
      type Out = m.Out

      def inject[V](tv: T[V]): Union[R, V] =
        m.inject(Tag(tv))

      def project[V](u: Union[R, V]): Option[T[V]] =
        m.project(u).map(Tag.unwrap)
    }

  /**
   * Syntactic sugar for the Member type
   *
   * implicit m: Member[M, R]
   * implicit m: M <= R
   *
   */
  type <=[M[_], R] = Member[M, R]

}


/**
 * The rank of a member effects is modelled as a type-level natural
 * and modelled by a value of that type
 */
trait MemberNat[T[_], R <: Effects] {
  type Out <: Effects

  def inject[V](effect: T[V]): Union[R, V]

  def project[V](union: Union[R, V]): Option[T[V]]
}

object MemberNat {

  type Aux[T[_], R <: Effects, U <: Effects] = MemberNat[T, R] { type Out = U }

  def infer[T[_], R <: Effects, U <: Effects](implicit m: MemberNat.Aux[T, R, U]): MemberNat.Aux[T, R, U] =
    m

  implicit def ZeroMemberNat[T[_], R <: Effects]: MemberNat.Aux[T, T |: R, R] = new MemberNat[T, T |: R] {
   type Out = R

    def inject[V](effect: T[V]): Union[T |: R, V] =
      Union.now(effect)

    def project[V](union: Union[T |: R, V]): Option[T[V]] =
      union match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }

  implicit def SuccessorMemberNat[T[_], O[_], R <: Effects](implicit m: MemberNat[T, R]): MemberNat.Aux[T, O |: R, O |: m.Out] = new MemberNat[T, O |: R] {
    type Out = O |: m.Out

    def inject[V](effect: T[V]) =
      Union.next(m.inject[V](effect))

    def project[V](union: Union[O |: R, V]) =
      union match {
        case UnionNow(_) => None
        case UnionNext(u) => m.project[V](u)
      }
  }

}

/**
 * type level naturals
 */
sealed trait Nat

trait Zero extends Nat
trait Succ[N <: Nat] extends Nat

/**
 * values with a phantom type representing a type-level natural
 */
case class P[N <: Nat]()

object P {

  implicit def ZeroPredicate: P[Zero] =
    P[Zero]

  implicit def SuccPredicate[N <: Nat](implicit prev: P[N]): P[Succ[N]] =
    P[Succ[N]]
}

