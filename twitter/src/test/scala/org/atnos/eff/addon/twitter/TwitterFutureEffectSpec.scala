package org.atnos.eff.addon.twitter

import scala.concurrent.duration._
import com.twitter.util.{Await, Future}

import cats.Eval
import TwitterFutureEffect._
import cats.implicits._
import org.atnos.eff._
import org.atnos.eff.allbutfuture._
import org.atnos.eff.syntax.all._
import org.specs2._
import org.specs2.concurrent.ExecutionEnv
import org.scalacheck._

import scala.collection.mutable.ListBuffer
import scala.concurrent._
import duration._

import org.specs2.matcher.ThrownExpectations

import scala.util.control._

class TwitterFutureEffectSpec(implicit ee: ExecutionEnv) extends Specification with ScalaCheck with ThrownExpectations { def is = s2"""

 Future effects can work as normal values                      $e1
 Future effects can be attempted                               $e2
 Future effects can be executed concurrently                   $e3
 Future effects are stacksafe with recursion                   $e6
 An Future effect can be created from Either                   $e7
 An Future forked computation can be timed out                 $e8

 Simple Future calls can be memoized                 $e9
 Attempted Future calls can be memoized              $e10
 Simple Future calls with timeout can be memoized    $e11
 Attempted Future calls with timeout can be memoized $e12

 TwitterTimedFuture calls can be memoized with a memo effect $e10


"""

  type S = Fx.fx2[TwitterTimedFuture, Option]

  def e1 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- futureDelay(10)
      b <- futureDelay(20)
    } yield a + b

    Await.result(action[S].runOption.detach.runNow(es)) must beSome(30)
  }

  def e2 = {
    def action[R :_future :_option]: Eff[R, Int] = for {
      a <- futureDelay(10)
      b <- futureDelay { boom; 20 }
    } yield a + b

    Await.result(action[S].twitterFutureAttempt.runOption.detach.runNow(es)) must beSome(beLeft(boomException))
  }

  def e3 = prop { ls: List[Int] =>
    val messages: ListBuffer[Int] = new ListBuffer[Int]

    def action(i: Int): Eff[S, Unit] =
      futureFork {
        Thread.sleep(i.toLong)
        messages.append(i)
      }

    val run = Eff.traverseA(ls)(action)

    eventually(retries = 5, sleep = 2.second) {
      messages.clear
      Await.result(run.runOption.detachA(TwitterTimedFuture.ApplicativeTwitterTimedFuture).runNow(es))

      "the messages are ordered" ==> {
        messages.toList ==== ls.sorted
      }
    }
  }.set(minTestsOk = 5).setGen(Gen.const(scala.util.Random.shuffle(List(10, 200, 300, 400, 500))))

  def e5 = {
    val list = (1 to 5000).toList
    type U = Fx.prepend[Choose, S]
    val action = list.traverseA(i => chooseFrom[U, Int](List(1)) >> futureDelay[U, String](i.toString))

    Await.result(action.runChoose[List].runOption.map(_.map(_.flatten)).detach.runNow(es)) must beSome(list.map(_.toString))
  }

  def e6 = {
    type R = Fx.fx1[TwitterTimedFuture]

    def loop(i: Int): Eff[R, Int] =
      if (i == 0) Eff.pure(1)
      else        futureDelay[R, Eff[R, Int]](loop(i - 1)).flatten[Int].map(_ + 1)

    eventually(retries = 5, sleep = 1.second) {
      Await.result(futureDelay(loop(100000)).detach.runNow(es)) must not(throwAn[Exception])
    }
  }

  def e7 = {
    Await.result(futureFromEither(Left[Throwable, Int](boomException)).twitterFutureAttempt.detach.runNow(es)) must beLeft(boomException)
  }

  def e8 = {
    lazy val slow = { sleepFor(200.millis); 1 }
    Await.result(futureDelay(slow, timeout = Some(50.millis)).twitterFutureAttempt.detach.runNow(es)) must beLeft
  }

  def e9 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }))

    Await.result((makeRequest >> makeRequest).detach.runNow(es)) must be_==(1)
    invocationsNumber must be_==(1)
  }

  def e10 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }))

    Await.result((makeRequest >> makeRequest).twitterFutureAttempt.detach.runNow(es)) must beRight(1)
    invocationsNumber must be_==(1)
  }

  def e11 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()
    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))

    Await.result((makeRequest >> makeRequest).detach.runNow(es)) must be_==(1)
    invocationsNumber must be_==(1)
  }

  def e12 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    def makeRequest = futureMemo("only once", cache, futureDelay({ invocationsNumber += 1; 1 }, timeout = Option(100.millis)))
    Await.result((makeRequest >> makeRequest).twitterFutureAttempt.detach.runNow(es)) must beRight(1)

    invocationsNumber must be_==(1)
  }

  def e13 = {
    var invocationsNumber = 0
    val cache = ConcurrentHashMapCache()

    type S = Fx.fx2[Memoized, TwitterTimedFuture]
    def makeRequest = futureMemoized("only once", futureDelay[S, Int]({ invocationsNumber += 1; 1 }))

    Await.result((makeRequest >> makeRequest).runTwitterFutureMemo(cache).detach.runNow(es)) must be_==(1)
    invocationsNumber must be_==(1)
  }

  /**
    * HELPERS
    */

  def boom: Unit = throw boomException
  val boomException: Throwable = new Exception("boom")

  val es = ExecutorServices.create(ee.executorService, ee.scheduledExecutorService)
  
  def sleepFor(duration: FiniteDuration) =
    try Thread.sleep(duration.toMillis) catch { case t: Throwable => () }
}

