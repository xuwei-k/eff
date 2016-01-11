package org.specs2.site

import org.specs2.control.eff._
import Eff._
import Effects._
import EvalEffect._
import WriterEffect._
import snippets._, HadoopS3Snippet._
import HadoopStack._
import S3Stack.{WriterString=>_,_}
import cats.syntax.all._

object IntoStack extends UserGuidePage { def is = "Effect stacks".title ^ s2"""

We have seen, in the [open-closed](open-closed.md) section, that we can create effects for a given effect stack, for example
 to interact with a [Hadoop](https://hadoop.apache.org) cluster. We can also define another stack, for storing and retrieving data on [S3](https://aws.amazon.com/s3).
${definition[HadoopS3Snippet]}

So what happens when you want to both use S3 and Hadoop? As you can see from the definition above those 2 stacks share
some common effects, so the resulting stack we want to work with is:${snippet{
import HadoopStack._
import S3Stack.{WriterString=>_,_}

type HadoopS3 = S3Reader |: HadoopReader |: WriterString |: Eval |: NoEffect
}}

Then we can use the `into` method to inject effects from each stack into this common stack:${snippet{

// this imports the `into` syntax
import org.specs2.control.eff.syntax.eff._

val action = for {
  // read a file from hadoop
  s <- readFile("/tmp/data").into[HadoopS3]

  // write a file on S3
  _ <- writeFile("key", s)  .into[HadoopS3]
} yield ()

// and we can run the composite action
run(runEval(runWriter(runHadoopReader(HadoopConf(10))(runS3Reader(S3Conf("bucket"))(action)))))
}.eval}

You can find a fully working example of this approach in `src/test/org/specs2/example/StacksSpec`.
"""

}


