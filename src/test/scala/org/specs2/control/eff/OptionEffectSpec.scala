package org.specs2.control.eff

import cats.Eval
import com.ambiata.disorder._
import org.specs2.{ScalaCheck, Specification}
import Eff._
import Effects._
//import ReaderEffect._
import OptionEffect._

import cats.syntax.all._
import cats.std.all._
import cats.data._

class OptionEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the option monad                     $optionMonad
 run the option monad with nothing        $optionWithNothingMonad
 run the option monad with reader         $optionReader

 The Eff monad is stack safe with Option  $$stacksafeOption

"""

  def optionMonad = {
    type S = Option |: NoEffect

    val option: Eff[S, String] =
      for {
        s1 <- OptionEffect.some[S, String]("hello")
        s2 <- OptionEffect.some[S, String]("world")
      } yield s1 + " " + s2

    run(runOption(option)) === Some("hello world")
  }

  def optionWithNothingMonad = {
    type S = Option |: NoEffect

    val option: Eff[S, String] =
      for {
        s1 <- OptionEffect.some[S, String]("hello")
        s2 <- OptionEffect.none[S, String]
      } yield s1 + " " + s2

    run(runOption(option)) === None
  }

  def optionReader = prop { (init: PositiveIntSmall, someValue: PositiveIntSmall) =>

    // define a Reader / Option stack
    type R[A] = Reader[Int, A]
    type S = Option |: R |: NoEffect

//    implicit def RMember: Member.Aux[R, S, Option |: NoEffect] =
//      ??? //Member.SuccessorMember[R, Option, R |: NoEffect]
//
//    implicit def RMemberZ[U <: Effects]: Member.Aux[R, U, R |: U] =
//      ???

    // create actions
//    val readOption: Eff[S, Int] =
//      for {
//        j <- OptionEffect.some[S, Int](someValue.value)
//        i <- ask[S, Int]
//      } yield i + j
//
//    // run effects
//    val initial = init.value
//
//    run(runReader(initial)(runOption(readOption))) must_==
//      Some(initial + someValue.value)
  ok
  }


//  def stacksafeOption = {
//    type E = Option |: NoEffect
//
//    val list = (1 to 5000).toList
//    val action = list.traverseU(i => OptionEffect.some(i))
//
//    run(runOption(action)) ==== Some(list)
//  }

}


object test {

  type RI[A] = Reader[Int, A]
  type S = Eval |: RI |: NoEffect

  def send2[T[_], R, V](tv: T[V])(implicit member: Member2[T, R]): Eff[R, V] =
    impure(member.inject(tv), Arrs.unit)

  def eval2[R, A](a: A)(implicit member: Member2[Eval, R]): Eff[R, A] =
    send2[Eval, R, A](Eval.later(a))

  def ask2[R, T](implicit member: Member2[Reader[T, ?], R]): Eff[R, T] =
    local2[R, T, T](identity)

  def local2[R, T, U](f: T => U)(implicit member: Member2[Reader[T, ?], R]): Eff[R, U] =
    send2[Reader[T, ?], R, U](Reader(f))

  def runReader2[R <: Effects, U <: Effects, A, B](env: A)(r: Eff[R, B])(implicit m: Member2.Aux[Reader[A, ?], R, U]): Eff[U, B] =
   ???

  def runOption2[R <: Effects, U <: Effects, A](r: Eff[R, A])(implicit m: Member2.Aux[Eval, R, U]): Eff[U, Option[A]] = ???

//   implicit def a: Member2.Aux[RI, Eval |: NoEffect, S] = ???
//   implicit def b: Member2.Aux[Eval, RI |: NoEffect, S] = ???
//   implicit def c: Member2.Aux[RI, NoEffect, RI |: NoEffect] = ???

   val readOption: Eff[S, Int] =
      for {
        j <- eval2[S, Int](3)
        i <- ask2[S, Int]
      } yield i + j

    // run effects
    val initial = 10

    run(runReader2(initial)(runOption2(readOption)))



}

trait Member2[T[_], R] {
  type Out <: Effects

  def inject[V](tv: T[V]): Union[R, V]

  def project[V](u: Union[R, V]): Option[T[V]]
}

object Member2 extends Lower2 {

  type Aux[T[_], U, R] = Member2[T, R] { type Out = U }

  implicit def SuccessorMember[T[_], O[_], R <: Effects](implicit o: Member2[O, O |: R], m: Member[T, R]): Member2.Aux[T, O |: m.Out, O |: R] = new Member2[T, O |: R] {
    type Out = O |: m.Out

    def inject[V](effect: T[V]) =
      Union.next(m.inject[V](effect))

    def project[V](union: Union[O |: R, V]) =
      union match {
        case UnionNow(_) => None
        case UnionNext(u) => m.project[V](u)
      }
  }



//  implicit def ZeroMember[T[_]]: Member2.Aux[T, NoEffect, T |: NoEffect] = new Member2[T, T |: NoEffect] {
//   type Out = NoEffect
//
//    def inject[V](effect: T[V]): Union[T |: NoEffect, V] =
//      Union.now(effect)
//
//    def project[V](union: Union[T |: NoEffect, V]): Option[T[V]] =
//      union match {
//        case UnionNow(x) => Some(x)
//        case _ => None
//      }
//  }
}
trait Lower2 {
  implicit def ZeroMemberR[T[_], R <: Effects]: Member2.Aux[T, R, T |: R] = new Member2[T, T |: R] {
   type Out = R

    def inject[V](effect: T[V]): Union[T |: R, V] =
      Union.now(effect)

    def project[V](union: Union[T |: R, V]): Option[T[V]] =
      union match {
        case UnionNow(x) => Some(x)
        case _ => None
      }
  }


}
