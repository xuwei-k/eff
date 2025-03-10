package org.atnos.eff
package syntax.addon.scalaz

import org.atnos.eff.addon.scalaz.*

object eff extends eff

trait eff extends org.atnos.eff.syntax.effOperations with effScalaz

trait effScalaz {
  given scalazExtension: AnyRef with {

    extension [M[_], A](e: Eff[Fx1[M], A]) {
      def detach(using M: scalaz.Monad[M], b: scalaz.BindRec[M]): M[A] =
        EffScalaz.detach(e)

      def detachA(applicative: scalaz.Applicative[M])(using monad: scalaz.Monad[M], bindRec: scalaz.BindRec[M]): M[A] =
        EffScalaz.detachA(e)(using monad, bindRec, applicative)
    }

    extension [F[_], A](values: F[A]) {
      def traverseA[R, B](f: A => Eff[R, B])(using F: scalaz.Traverse[F]): Eff[R, F[B]] =
        EffScalaz.traverseA(values)(f)

      def flatTraverseA[R, B](f: A => Eff[R, F[B]])(using F1: scalaz.Traverse[F], F2: scalaz.Bind[F]): Eff[R, F[B]] =
        EffScalaz.flatTraverseA(values)(f)
    }

    extension [F[_], R, A](values: F[Eff[R, A]]) {
      def sequenceA(using F: scalaz.Traverse[F]): Eff[R, F[A]] =
        EffScalaz.sequenceA(values)
    }

    extension [F[_], R, A](values: F[Eff[R, F[A]]]) {
      def flatSequenceA(using F1: scalaz.Traverse[F], F2: scalaz.Bind[F]): Eff[R, F[A]] =
        EffScalaz.flatSequenceA(values)
    }
  }

}
