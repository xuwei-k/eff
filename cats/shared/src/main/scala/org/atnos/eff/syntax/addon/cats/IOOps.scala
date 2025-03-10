package org.atnos.eff.syntax.addon.cats

import cats.effect.IO
import cats.effect.LiftIO
import cats.effect.unsafe.IORuntime
import org.atnos.eff._
import org.atnos.eff.addon.cats.effect.IOEffect
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import org.atnos.eff.Eff

final class IOOps[A](private val e: Eff[Fx1[IO], A]) extends AnyVal {

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
