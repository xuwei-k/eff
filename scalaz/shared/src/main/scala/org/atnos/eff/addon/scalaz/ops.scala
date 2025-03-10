package org.atnos.eff
package addon.scalaz

import scalaz._

final class EffScalazOneEffectOps[M[_], A](private val e: Eff[Fx1[M], A]) extends AnyVal {
  def detach(using M: Monad[M], b: BindRec[M]): M[A] =
    EffScalaz.detach(e)

  def detachA(applicative: Applicative[M])(using monad: Monad[M], bindRec: BindRec[M]): M[A] =
    EffScalaz.detachA(e)(using monad, bindRec, applicative)
}

final class EffScalazApplicativeOps[F[_], A](private val values: F[A]) extends AnyVal {
  def traverseA[R, B](f: A => Eff[R, B])(using F: Traverse[F]): Eff[R, F[B]] =
    EffScalaz.traverseA(values)(f)

  def flatTraverseA[R, B](f: A => Eff[R, F[B]])(using F1: Traverse[F], F2: Bind[F]): Eff[R, F[B]] =
    EffScalaz.flatTraverseA(values)(f)
}

final class EffScalazSequenceOps[F[_], R, A](private val values: F[Eff[R, A]]) extends AnyVal {
  def sequenceA(using F: Traverse[F]): Eff[R, F[A]] =
    EffScalaz.sequenceA(values)
}

final class EffScalazFlatSequenceOps[F[_], R, A](private val values: F[Eff[R, F[A]]]) extends AnyVal {
  def flatSequenceA(using F1: Traverse[F], F2: Bind[F]): Eff[R, F[A]] =
    EffScalaz.flatSequenceA(values)
}
