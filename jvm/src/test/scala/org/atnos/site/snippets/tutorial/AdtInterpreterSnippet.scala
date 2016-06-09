// 8<---
package org.atnos.site.snippets.tutorial

import AdtSnippet._
import AdtCreationSnippet._

trait AdtInterpreterSnippet {
// 8<---

import org.atnos.eff._, interpret._
import cats.implicits._
import cats.data._
import scala.collection.mutable._

/**
 * Unsafe interpreter for KVStore effects
 *
 * the program will crash if a type is incorrectly specified.
 *
 * The interpreter uses the `interpret1` method from `org.atnos.eff.Interpreter` to implement a
 * stack-safe interpretation of effects
 *
 * `interpret1` needs the definition of the recursion where
 * we get each `KVStore[X]` effect and decide if there is a value
 * we can use for further computations or if we can take an early exit.
 *
 * For example if we had an `XorLike` effect we would exit early when encountering `Left` values.
 */
def runKVStoreUnsafe[R <: Effects, A](effects: Eff[R, A])(implicit m: Member[KVStore, R]): Eff[m.Out, A] = {
  // a very simple (and imprecise) key-value store
  val kvs = Map.empty[String, Any]

  val recurse = new Recurse[KVStore, m.Out, A] {
    def apply[X](kv: KVStore[X]): X Xor Eff[m.Out, A] =
      kv match {
        case Put(key, value) =>
          println(s"put($key, $value)")
          Xor.left { kvs.put(key, value); ().asInstanceOf[X] }

        case Get(key) =>
          println(s"get($key)")
          Xor.left(kvs.get(key).asInstanceOf[X])

        case Delete(key) =>
          println(s"delete($key)")
          Xor.left { kvs.remove(key); ().asInstanceOf[X] }
      }
  }
  interpret1((a: A) => a)(recurse)(effects)(m)

}

  // 8<---
}

object AdtInterpreterSnippet extends AdtInterpreterSnippet
