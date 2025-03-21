package org.atnos.eff.syntax

import cats.*
import org.atnos.eff.*

object memo extends memo

trait memo {

  implicit def toMemoEffectOps[R, A](e: Eff[R, A]): MemoEffectOps[R, A] = new MemoEffectOps[R, A](e)

}

final class MemoEffectOps[R, A](private val e: Eff[R, A]) extends AnyVal {

  def runMemo[U](cache: Cache)(implicit member: Member.Aux[Memoized, R, U], eval: Eval |= U): Eff[U, A] =
    MemoEffect.runMemo(cache)(e)(using member, eval)

  def runFutureMemo[U](cache: Cache)(implicit memMember: Member.Aux[Memoized, R, U], futMember: TimedFuture |= U): Eff[U, A] =
    MemoEffect.runFutureMemo(cache)(e)(using memMember, futMember)

}
