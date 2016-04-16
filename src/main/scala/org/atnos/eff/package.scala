package org.atnos

package object eff {

  type <=[M[_], R] = Member.<=[M, R]

  object all extends
    ReaderEffect with
    WriterEffect with
    StateEffect with
    EvalEffect with
    OptionEffect with
    ListEffect with
    DisjunctionEffect with
    ChooseEffect with
    EffInterpretation with
    EffCreation with
    EffImplicits with
    Effects

  object interpret extends
    Interpret
}
