// 8<---
package org.specs2.site.snippets

import HadoopS3Snippet._
import HadoopStack._
import S3Stack.{WriterString=>_,_}

trait HadoopS3Snippet {
// 8<---

import org.specs2.control.eff._
import Eff._
import Effects._
import EvalEffect._
import WriterEffect._
import cats.data._
import cats.syntax.all._
import Tag._

object HadoopStack {
  trait HadoopTag
  case class HadoopConf(mappers: Int)

  type HadoopReader[A] = Reader[HadoopConf, A] @@ HadoopTag
  type WriterString[A] = Writer[String, A]
  type Hadoop = HadoopReader |: WriterString |: Eval |: NoEffect

  implicit def HadoopReaderMember: Member[HadoopReader, Hadoop] =
    Member.infer

  implicit def WriterStringMember: Member[WriterString, Hadoop] =
    Member.infer

  def askHadoopConf: Eff[Hadoop, HadoopConf] =
    ReaderEffect.askTagged

  def readFile(path: String): Eff[Hadoop, String] =
    for {
      c <- askHadoopConf
      _ <- tell("Reading from "+path)
    } yield c.mappers.toString

  def runHadoopReader[R <: Effects, A](conf: HadoopConf): Eff[HadoopReader |: R, A] => Eff[R, A] =
    (e: Eff[HadoopReader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)

}

object S3Stack {
  trait S3Tag
  case class S3Conf(bucket: String)

  type S3Reader[A] = Reader[S3Conf, A] @@ S3Tag
  type WriterString[A] = Writer[String, A]

  type S3 = S3Reader |: WriterString |: Eval |: NoEffect

  implicit def S3ReaderMember: Member[S3Reader, S3] =
    Member.infer

  implicit def WriterStringMember: Member[WriterString, S3] =
    Member.infer

  def askS3Conf: Eff[S3, S3Conf] =
    ReaderEffect.askTagged

  def writeFile(key: String, content: String): Eff[S3, Unit] =
    for {
      c <- askS3Conf
      _ <- tell("Writing to bucket "+c.bucket+": "+content)
    } yield ()

  def runS3Reader[R <: Effects, A](conf: S3Conf): Eff[S3Reader |: R, A] => Eff[R, A] =
    (e: Eff[S3Reader |: R, A]) => ReaderEffect.runTaggedReader(conf)(e)
}

// 8<---

  type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect

}

object HadoopS3Snippet extends HadoopS3Snippet
