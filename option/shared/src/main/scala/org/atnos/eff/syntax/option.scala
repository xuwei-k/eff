package org.atnos.eff.syntax

import org.atnos.eff._

object option extends option

trait option {

  implicit class OptionEffectOps[R, A](e: Eff[R, A]) {

    def runOption(using member: Member[Option, R]): Eff[member.Out, Option[A]] =
      OptionInterpretation.runOption(e)(using member.aux)

    def runOptionU[U](using member: Member.Aux[Option, R, U]): Eff[U, Option[A]] =
      OptionInterpretation.runOption(e)(using member.aux)

  }

}
