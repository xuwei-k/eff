package org.atnos.eff.syntax

import cats.Semigroup
import org.atnos.eff.*

object either extends either

trait either {
  implicit final def toEitherEffectOps[R, A](e: Eff[R, A]): EitherEffectOps[R, A] = new EitherEffectOps[R, A](e)
}

final class EitherEffectOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runEither[E](implicit m: Member[Either[E, *], R]): Eff[m.Out, Either[E, A]] =
    EitherInterpretation.runEither[R, m.Out, E, A](e)(using m)

  def runEitherU[E, U](implicit m: Member.Aux[Either[E, *], R, U]): Eff[U, Either[E, A]] =
    EitherInterpretation.runEither(e)(using m)

  def runEitherCombine[E, U](implicit m: Member.Aux[Either[E, *], R, U], s: Semigroup[E]): Eff[U, Either[E, A]] =
    EitherInterpretation.runEitherCombine(e)(using m, s)

  def attemptEither[E](implicit m: Either[E, *] /= R): Eff[R, Either[E, A]] =
    EitherInterpretation.attemptEither(e)(using m)

  def catchLeft[E](handle: E => Eff[R, A])(implicit member: Either[E, *] /= R): Eff[R, A] =
    EitherInterpretation.catchLeft(e)(handle)(using member)

  def runEitherCatchLeft[E, U](handle: E => Eff[U, A])(implicit member: Member.Aux[Either[E, *], R, U]): Eff[U, A] =
    EitherInterpretation.runEitherCatchLeft(e)(handle)(using member)

  def catchLeftCombine[E](handle: E => Eff[R, A])(implicit member: Either[E, *] /= R, s: Semigroup[E]): Eff[R, A] =
    EitherInterpretation.catchLeftCombine(e)(handle)(using member, s)

  def zoomEither[BR, U1, U2, C, B](
    getter: C => B
  )(implicit m1: Member.Aux[Either[C, *], R, U1], m2: Member.Aux[Either[B, *], BR, U2], into: IntoPoly[U1, U2]): Eff[BR, A] =
    EitherInterpretation.zoomEither[R, BR, U1, U2, C, B, A](e, getter)

  def translateEither[U, C, B](getter: C => B)(implicit sr: Member.Aux[Either[C, *], R, U], br: Either[B, *] |= U): Eff[U, A] =
    EitherInterpretation.translateEither[R, U, C, B, A](e, getter)

  def localEither[E](modify: E => E)(implicit m: Either[E, *] /= R): Eff[R, A] =
    EitherInterpretation.localEither[R, E, A](e)(modify)
}
