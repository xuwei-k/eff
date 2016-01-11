// 8<---
package org.specs2.site.snippets

import cats.data.Xor
import cats.data.Xor.Left
import org.specs2.control.eff.Effects._
import org.specs2.control.eff.Interpret._
import org.specs2.control.eff.{Effects, Eff}
import org.specs2.control.eff.Eff._
import org.specs2.control.eff.Member._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

trait FutureEffectSnippet {

// 8<---
object FutureEffect {
  type Fut[A] = Future[() => A]

  def future[R, A](a: => A)(implicit m: Fut <= R): Eff[R, A] =
    send[Fut, R, A](Future(() => a))

  def runFuture[R <: Effects, A, B](atMost: Duration)(effects: Eff[Fut |: R, A]): Eff[R, A] = {
    val recurse = new Recurse[Fut, R, A] {
      def apply[X](m: Fut[X]): X Xor Eff[R, A] =
        Left(Await.result(m.map(_ ()), atMost))
    }
    interpret1((a: A) => a)(recurse)(effects)
  }
}

// 8<---
}

object FutureEffectSnippet extends FutureEffectSnippet
