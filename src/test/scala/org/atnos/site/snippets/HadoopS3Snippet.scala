// 8<---
package org.atnos.site.snippets

trait HadoopS3Snippet {
// 8<---

import org.atnos.eff._, all._
import cats.data._
import cats.syntax.all._
import Tag._

object HadoopStack {
  object Hadoop {

    trait HadoopTag
    case class HadoopConf(mappers: Int)

    type HadoopReader[A] = Reader[HadoopConf, A] @@ HadoopTag

    type Hadoop = HadoopReader |: Writer[String, ?] |: Eval |: NoEffect

    implicit val HadoopReaderMember: Member.Aux[HadoopReader, Hadoop, Writer[String, ?] |: Eval |: NoEffect] =
      Member.first

    implicit val WriterStringMember: Member.Aux[Writer[String, ?], Hadoop, HadoopReader |: Eval |: NoEffect] =
      Member.successor

    implicit val EvalMember: Member.Aux[Eval, Hadoop, HadoopReader |: Writer[String, ?] |: NoEffect] =
      Member.successor
  }


  import Hadoop._

  def askHadoopConf: Eff[Hadoop, HadoopConf] =
    ReaderEffect.askTagged

  def readFile(path: String): Eff[Hadoop, String] =
    for {
      c <- askHadoopConf
      _ <- tell("Reading from "+path)
    } yield c.mappers.toString

}

object S3Stack {
  object S3 {
    trait S3Tag
    case class S3Conf(bucket: String)

    type S3Reader[A] = Reader[S3Conf, A] @@ S3Tag

    type S3 = S3Reader |: Writer[String, ?] |: Eval |: NoEffect

    implicit val S3ReaderMember: Member.Aux[S3Reader, S3, Writer[String, ?] |: Eval |: NoEffect] =
      Member.first

    implicit val WriterStringMember: Member.Aux[Writer[String, ?], S3, S3Reader |: Eval |: NoEffect] =
      Member.successor

    implicit val EvalMember: Member.Aux[Eval, S3, S3Reader |: Writer[String, ?] |: NoEffect] =
      Member.successor
  }

  import S3._

  def askS3Conf: Eff[S3, S3Conf] =
    ReaderEffect.askTagged

  def writeFile(key: String, content: String): Eff[S3, Unit] =
    for {
      c <- askS3Conf
      _ <- tell("Writing to bucket "+c.bucket+": "+content)
    } yield ()
}

// 8<---

  object HadoopS3 {
    import HadoopStack.Hadoop._
    import S3Stack.S3._

    type HadoopS3 = S3Reader |: HadoopReader |: Writer[String, ?] |: Eval |: NoEffect

    implicit val S3ReaderMember: Member.Aux[S3Reader, HadoopS3, HadoopReader |: Writer[String, ?] |: Eval |: NoEffect] =
      Member.first

    implicit val HadoopReaderMember: Member.Aux[HadoopReader, HadoopS3, S3Reader |: Writer[String, ?] |: Eval |: NoEffect] =
      Member.successor

    implicit val WriterStringMember: Member.Aux[Writer[String, ?], HadoopS3, S3Reader |: HadoopReader |: Eval |: NoEffect] =
      Member.successor

    implicit val EvalMember: Member.Aux[Eval, HadoopS3, S3Reader |: HadoopReader |: Writer[String, ?] |: NoEffect] =
      Member.successor

  }

}

object HadoopS3Snippet extends HadoopS3Snippet

