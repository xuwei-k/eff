package org.atnos.eff.syntax

import cats._
import org.atnos.eff._

object eval extends eval

trait eval {

  given evalExtension: AnyRef with {
    extension [R, A](e: Eff[R, A]) {
      def runEval(using member: Member[Eval, R]): Eff[member.Out, A] =
        EvalInterpretation.runEval(e)(using member.aux)

      def attemptEval(using member: Member[Eval, R]): Eff[member.Out, Throwable Either A] =
        EvalInterpretation.attemptEval(e)(using member.aux)
    }
  }

}
