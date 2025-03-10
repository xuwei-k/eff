package org.atnos.eff.syntax.addon.cats

import cats.effect.IO
import cats.effect.LiftIO
import cats.effect.unsafe.IORuntime
import org.atnos.eff.*
import org.atnos.eff.addon.cats.effect.IOEffect
import org.atnos.eff.addon.cats.effect.IOInterpretation
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

object effect {

  given catsEffectExtension: AnyRef with {

    extension [A](e: Eff[Fx1[IO], A]) {

      def unsafeRunAsync(cb: Either[Throwable, A] => Unit)(using i: IORuntime): Unit =
        IOEffect.unsafeRunAsync(e)(cb)

      def unsafeRunSync(using i: IORuntime): A =
        IOEffect.unsafeRunSync(e)

      def unsafeRunTimed(limit: FiniteDuration)(using i: IORuntime): Option[A] =
        IOEffect.unsafeRunTimed(e, limit)

      def unsafeToFuture(using i: IORuntime): Future[A] =
        IOEffect.unsafeToFuture(e)

      def to[F[_]](using f: LiftIO[F]): F[A] =
        IOEffect.to(e)

    }

    extension [R, A](e: Eff[R, A]) {

      def ioAttempt(using m: MemberInOut[IO, R]): Eff[R, Either[Throwable, A]] =
        IOEffect.ioAttempt(e)

      def runIoMemo[U](cache: Cache)(using m: Member.Aux[Memoized, R, U], task: IO |= U): Eff[U, A] =
        IOEffect.runIoMemo(cache)(e)

      def ioMemo(key: AnyRef, cache: Cache)(using task: IO /= R): Eff[R, A] =
        IOInterpretation.ioMemo(key, cache, e)
    }
  }

}
