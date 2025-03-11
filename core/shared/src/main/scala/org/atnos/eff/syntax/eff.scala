package org.atnos.eff
package syntax

import cats._
import cats.data.Writer

/**
  * Operations of Eff[R, A] values
  */
object eff extends eff

trait eff extends effOperations with effCats

trait effOperations {
  given effExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def into[U](using f: IntoPoly[R, U]): Eff[U, A] =
        Eff.effInto(e)(using f)

      def transform[BR, U, M[_], N[_]](t: ~>[M, N])(using m: Member.Aux[M, R, U], n: Member.Aux[N, BR, U]): Eff[BR, A] =
        Interpret.transform(e, t)(using m, n, IntoPoly.intoSelf[U])

      def translate[M[_], U](t: Translate[M, U])(using m: Member.Aux[M, R, U]): Eff[U, A] =
        Interpret.translate(e)(t)(using m)

      def translateInto[T[_], U](t: Translate[T, U])(using m: MemberInOut[T, R], into: IntoPoly[R, U]): Eff[U, A] =
        interpret.translateInto(e)(t)(using m, into)

      def write[T[_], O](w: Write[T, O])(using memberT: MemberInOut[T, R], memberW: MemberInOut[Writer[O, *], R]): Eff[R, A] =
        interpret.write(e)(w)

      def augment[T[_], O[_]](w: Augment[T, O])(using memberT: MemberInOut[T, R], memberO: MemberIn[O, R]): Eff[R, A] =
        interpret.augment(e)(w)

      def runPure: Option[A] =
        Eff.runPure(e)

      def tuple2[B](b: Eff[R, B]): Eff[R, (A, B)] =
        Eff.EffApplicative[R].tuple2(e, b)
    }

    extension [A](e: Eff[NoFx, A]) {
      def run: A =
        Eff.run(e)
    }

  }

  implicit final def toEffSendOps[M[_], A](ma: M[A]): EffSendOps[M, A] = new EffSendOps(ma)
  implicit final def toEffPureOps[A](a: A): EffPureOps[A] = new EffPureOps(a)
}

trait effCats {
  implicit final def toEffOneEffectOps[M[_], A](e: Eff[Fx1[M], A]): EffOneEffectOps[M, A] = new EffOneEffectOps(e)
  implicit final def toEffMonadicOps[R, M[_], A](e: Eff[R, M[A]]): EffMonadicOps[R, M, A] = new EffMonadicOps(e)
  implicit final def toEffApplicativeOps[F[_], A](values: F[A]): EffApplicativeOps[F, A] = new EffApplicativeOps(values)
  implicit final def toEffSequenceOps[F[_], R, A](values: F[Eff[R, A]]): EffSequenceOps[F, R, A] = new EffSequenceOps(values)
  implicit final def toEffFlatSequenceOps[F[_], R, A](values: F[Eff[R, F[A]]]): EffFlatSequenceOps[F, R, A] = new EffFlatSequenceOps(values)
}

final class EffSendOps[M[_], A](private val ma: M[A]) extends AnyVal {
  def send[R](using m: M |= R): Eff[R, A] = Eff.send(ma)
}

final class EffPureOps[A](private val a: A) extends AnyVal {
  def pureEff[R]: Eff[R, A] =
    Eff.pure(a)
}

final class EffOneEffectOps[M[_], A](private val e: Eff[Fx1[M], A]) extends AnyVal {
  def detach[E](using M: MonadError[M, E]): M[A] =
    Eff.detach(e)

  def detachA[E](applicative: Applicative[M])(using monad: MonadError[M, E]): M[A] =
    Eff.detachA(e)(using monad, applicative)
}

final class EffMonadicOps[R, M[_], A](private val e: Eff[R, M[A]]) extends AnyVal {
  def collapse(using m: M |= R): Eff[R, A] =
    Eff.collapse[R, M, A](e)
}

final class EffApplicativeOps[F[_], A](private val values: F[A]) extends AnyVal {
  def traverseA[R, B](f: A => Eff[R, B])(using F: Traverse[F]): Eff[R, F[B]] =
    Eff.traverseA(values)(f)

  def flatTraverseA[R, B](f: A => Eff[R, F[B]])(using F1: Traverse[F], F2: FlatMap[F]): Eff[R, F[B]] =
    Eff.flatTraverseA(values)(f)
}

final class EffSequenceOps[F[_], R, A](private val values: F[Eff[R, A]]) extends AnyVal {
  def sequenceA(using F: Traverse[F]): Eff[R, F[A]] =
    Eff.sequenceA(values)
}

final class EffFlatSequenceOps[F[_], R, A](private val values: F[Eff[R, F[A]]]) extends AnyVal {
  def flatSequenceA(using F1: Traverse[F], F2: FlatMap[F]): Eff[R, F[A]] =
    Eff.flatSequenceA(values)
}
