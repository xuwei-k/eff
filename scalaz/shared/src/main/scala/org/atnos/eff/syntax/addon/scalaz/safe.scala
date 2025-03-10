package org.atnos.eff
package syntax.addon.scalaz

import scalaz.*

object safe extends org.atnos.eff.syntax.safe with safe

trait safe {

  given scalazSafeExtension: AnyRef with {

    extension [R, A](e: Eff[R, A]) {

      def runSafeDisjunction[U](using m: Member.Aux[Safe, R, U]): Eff[U, (Throwable \/ A, List[Throwable])] =
        addon.scalaz.safe.runSafeDisjunction(e)

      def execSafeDisjunction[U](using m: Member.Aux[Safe, R, U]): Eff[U, Throwable \/ A] =
        addon.scalaz.safe.execSafeDisjunction(e)

      def attemptSafeDisjunction(using m: Safe /= R): Eff[R, (Throwable \/ A, List[Throwable])] =
        addon.scalaz.safe.attemptSafeDisjunction(e)

      def attemptDisjunction(using member: MemberInOut[Safe, R]): Eff[R, Throwable \/ A] =
        addon.scalaz.safe.attemptDisjunction(e)

    }
  }
}
