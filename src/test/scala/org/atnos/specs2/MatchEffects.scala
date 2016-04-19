package org.atnos.specs2

import org.specs2.Specification
import org.atnos.eff._
import all._
import cats.arrow.NaturalTransformation
import cats.data._
import cats.~>
import org.specs2.matcher._
import org.specs2.execute._
import syntax.all._
import cats.syntax.all._

class MatchEffects extends Specification { def is = s2"""

 effects and interpreters can be used to nest checks $test

"""

  import MatchStack._

  def test = {
    val e = Option(List[Either[String, Int]](Right(2), Right(3)))

    runResult {
      for {
        as <- (e must beSome).opt
        a <- fromList(as)
        i <- (a must beRight).opt
        _ <- (i must be_>(0)).check
      } yield ()
    }

  }
}

import ResultLogicalCombinators._

object MatchStack {
  type SM[A] = State[Result, A]
  type WS[A] = Writer[String, A]

  type S = Option |: List |: WS |: SM |: NoEffect

  implicit def OptionMember: Member.Aux[Option, S, List |: WS |: SM |: NoEffect] =
    Member.first

  implicit def ListMember: Member.Aux[List, S, Option |: WS |: SM |: NoEffect] =
    Member.successor

  implicit def StateMember: Member.Aux[SM, S, Option |: List |: WS |: NoEffect] =
    Member.successor

  implicit def WriterMember: Member.Aux[WS, S, Option |: List |: SM |: NoEffect] =
    Member.successor

  def runResult[T](eff: Eff[S, T]): Result = {
    val ((_, ls), r) =
      eff.runOption.runList.runWriter.runState(Success(): Result).run
    r.updateMessage(ls.mkString("Context\n  ", "\n  ", "\n  "+r.message))
  }

  implicit def optionRefl: Option ~> Option =
    NaturalTransformation.id[Option]

  implicit def idOption: cats.Id ~> Option = new NaturalTransformation[cats.Id, Option] {
    def apply[A](a: cats.Id[A]): Option[A] =
      Option(a)
  }

  implicit def XorOptionNat[E]: (E Xor ?) ~> Option = new NaturalTransformation[(E Xor ?), Option] {
    def apply[A](e: E Xor A): Option[A] =
      e.toOption
  }

  implicit def EitherOptionNat[E]: (E Either ?) ~> Option = new NaturalTransformation[(E Either ?), Option] {
    def apply[A](e: E Either A): Option[A] =
      e.right.toOption
  }

  implicit class LiftedSimple[T](mr: MatchResult[T]) {
    def check[R](implicit o: Option <= R, s: State[Result, ?] <= R, w: WS <= R): Eff[R, T] =
      liftOption(mr, Option(mr.expectable.value))
  }

  implicit class Lifted[M[_], T](mr: MatchResult[M[T]])(implicit nat: M ~> Option) {
    def opt[R](implicit o: Option <= R, s: State[Result, ?] <= R, w: WS <= R): Eff[R, T] =
      lift(mr)
  }

  def lift[R, M[_], T](mr: MatchResult[M[T]])(implicit o: Option <= R, s: SM <= R, w: WS <= R, nat: M ~> Option): Eff[R, T] =
    liftOption(mr, nat(mr.expectable.value))

  def liftOption[R, T, U](mr: MatchResult[T], u: Option[U])(implicit o: Option <= R, s: SM <= R, w: WS <= R): Eff[R, U] =
    if (mr.isSuccess)
      tell(mr.expectable.value.toString+": OK")(w) >>
        option(u)(o)
    else
      modify((r: Result) => r and AsResult(mr))(s) >>
        OptionEffect.none(o)

  def extract[T, U](t: T)(f: PartialFunction[T, U]): Option[U] =
    f.lift(t)
}







