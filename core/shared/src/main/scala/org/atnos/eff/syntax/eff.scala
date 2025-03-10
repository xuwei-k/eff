package org.atnos.eff
package syntax

import cats.*
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
    }

    extension [A](e: Eff[NoFx, A]) {
      def run: A =
        Eff.run(e)
    }

    extension [M[_], A](ma: M[A]) {
      def send[R](using m: M |= R): Eff[R, A] = Eff.send(ma)
    }

    extension [A](a: A) {
      def pureEff[R]: Eff[R, A] =
        Eff.pure(a)
    }

  }
}

trait effCats {
  given effCatsExtension: AnyRef with {
    extension [M[_], A](e: Eff[Fx1[M], A]) {
      def detach[E](using M: MonadError[M, E]): M[A] =
        Eff.detach(e)

      def detachA[E](applicative: Applicative[M])(using monad: MonadError[M, E]): M[A] =
        Eff.detachA(e)(using monad, applicative)
    }

    extension [R, M[_], A](e: Eff[R, M[A]]) {
      def collapse(using m: M |= R): Eff[R, A] =
        Eff.collapse[R, M, A](e)
    }

    extension [M[_], A](ma: M[A]) {
      def traverseA[R, B](f: A => Eff[R, B])(using F: Traverse[M]): Eff[R, M[B]] =
        Eff.traverseA(ma)(f)

      def flatTraverseA[R, B](f: A => Eff[R, M[B]])(using F1: Traverse[M], F2: FlatMap[M]): Eff[R, M[B]] =
        Eff.flatTraverseA(ma)(f)
    }

    extension [F[_], R, A](values: F[Eff[R, A]]) {
      def sequenceA(using F: Traverse[F]): Eff[R, F[A]] =
        Eff.sequenceA(values)
    }

    extension [F[_], R, A](values: F[Eff[R, F[A]]]) {
      def flatSequenceA(using F1: Traverse[F], F2: FlatMap[F]): Eff[R, F[A]] =
        Eff.flatSequenceA(values)
    }

    extension [R, A](e: Eff[R, A]) {
      def tuple2[B](b: Eff[R, B]): Eff[R, (A, B)] =
        Eff.EffApplicative[R].tuple2(e, b)
    }
  }
}
