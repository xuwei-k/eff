// 8<---
package org.atnos.site.snippets.tutorial
import AdtSnippet._
import org.atnos.eff._

trait AdtCreationSnippet {
// 8<---

import cats.implicits._

type _KVStore[R] = Member[KVStore, R]

/** put returns nothing (i.e. Unit) */
def put[T, R :_KVStore](key: String, value: T): Eff[R, Unit] =
  Eff.send[KVStore, R, Unit](Put(key, value))

/** get returns a T value if the key exists */
def get[T, R :_KVStore](key: String): Eff[R, Option[T]] =
  Eff.send[KVStore, R, Option[T]](Get(key))

/** delete returns nothing (i.e. Unit) */
def delete[T, R :_KVStore](key: String): Eff[R, Unit] =
  Eff.send(Delete(key))

/** update composes get and set, and returns nothing. */
def update[T, R :_KVStore](key: String, f: T => T): Eff[R, Unit] =
  get[T, R](key).map(_.map(f)).void

  // 8<---
}

object AdtCreationSnippet extends AdtCreationSnippet
