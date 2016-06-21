package org.atnos.eff

import org.specs2._
import LazyStateEffect._
import all._
import cats.data.Xor
import cats._
import interpret._
import syntax.all._
import cats.implicits._

class LazyStateEffectSpec extends Specification { def is = s2"""

 ones should not overflow $makeOnes

"""

  def makeOnes = {
    type LS[A] = LazyState[Stream[Int], A]

    def ones[R](implicit m: LS <= R): Eff[R, Unit] =
      onDemand[R, Stream[Int], Unit](ones) >>
      modifyLazy[R, Stream[Int]](s => Stream.cons(1, s))

    lazy val onesS = ones[LS |: cats.Eval |: NoEffect]

    execLazyState(onesS)(Stream.empty[Int]).runEval.run.take(5).toList === List(1, 1, 1, 1, 1)
  }


}



object LazyStateEffect {
  trait LazyState[S, A]

  case class Get[S]() extends LazyState[S, S]
  case class Put[S](s: S) extends LazyState[S, Unit]
  case class Delay[S, A](e: () => Eff[LazyState[S, ?] |: cats.Eval |: NoEffect, A]) extends LazyState[S, A]

  def getLazy[R, S](implicit m: LazyState[S, ?] <= R): Eff[R, S] =
    send[LazyState[S, ?], R, S](Get())

  def putLazy[R, S](s: S)(implicit m: LazyState[S, ?] <= R): Eff[R, Unit] =
    send[LazyState[S, ?], R, Unit](Put(s))

  def modifyLazy[R, S](f: (=>S) => S)(implicit m: LazyState[S, ?] <= R): Eff[R, Unit] =
    getLazy[R, S] >>= ((s: S) => putLazy[R, S](f(s)))

  def onDemand[R, S, A](e: =>Eff[LazyState[S, ?] |: cats.Eval |: NoEffect, A])(implicit m: LazyState[S, ?] <= R): Eff[R, A] =
    send[LazyState[S, ?], R, A](Delay(() => e))

  def evalLazyState[R <: Effects, U <: Effects, S1, A](e: Eff[R, A])(initial: S1)(implicit m: Member.Aux[LazyState[S1, ?], R, U], ev: cats.Eval <= R): Eff[U, A] =
    runLazyState(e)(initial).map(_._1)

  def execLazyState[R <: Effects, U <: Effects, S1, A](e: Eff[R, A])(initial: S1)(implicit m: Member.Aux[LazyState[S1, ?], R, U], ev: cats.Eval <= R): Eff[U, S1] =
    runLazyState(e)(initial).map(_._2)

  def runLazyState[R <: Effects, U <: Effects, S, A](e: Eff[R, A])(initial: S)(implicit m: Member.Aux[LazyState[S, ?], R, U], ev: cats.Eval <= R): Eff[U, (A, S)] = {
    def go(effect: Eff[R, A], s: =>S): Eff[U, (A, S)] =
      effect match {
        case Pure(a) =>
          EffMonad.pure((a, s))

        case Impure(u, c) =>
          m.project(u) match {
            case Xor.Left(u1) =>
              Impure(u1, Arrs.singleton((x: u.X) => runLazyState(c(x))(s)(m, ev)))

            case Xor.Right(ss) =>
              ss match {
                case g: Get[_] =>
                  go(c(s), s)

                case Delay(eff1) =>
                  lazy val (a, s1) = runLazyState(eff1())(s).runEval.run
                  go(EvalEffect.delay(c(a)).flatMap(ca => ca), s1)

                case p =>
                  go(c(()), p.asInstanceOf[Put[S]].s)
              }

          }

        case i @ ImpureAp(u, c) =>
          go(i.toMonadic, s)

      }

    go(e, initial)
  }

//    val recurse = new StateRecurse[LazyState[S1, ?], A, (A, S1)] {
//      type S = S1
//      val init: S = initial
//      def finalize(a: A, s: S): (A, S) = (a, s)
//
//      def apply[X](ls: LazyState[S1, X], s: S): (X, S) =
//        ls match {
//          case Get() => (s.asInstanceOf[X], s)
//          case Put(newS) => (().asInstanceOf[X], newS)
//          case Delay(eff1) => runLazyState(eff1)(s).run
//        }
//    }
//
//    interpretState1((a: A) => (a, initial))(recurse)(e)

}
