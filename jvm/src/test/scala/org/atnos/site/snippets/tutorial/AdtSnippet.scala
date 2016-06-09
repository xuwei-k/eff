// 8<---
package org.atnos.site.snippets.tutorial

trait AdtSnippet {
// 8<---

sealed trait KVStore[+A]

case class Put[T](key: String, value: T) extends KVStore[Unit]
case class Get[T](key: String) extends KVStore[Option[T]]
case class Delete(key: String) extends KVStore[Unit]

// 8<---

}

object AdtSnippet extends AdtSnippet
