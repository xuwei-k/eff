package org.atnos.eff

import org.specs2.io.FilePathReader

trait Specs2Compat {

  protected final def readFileSpecs2(path: FilePath) =
    FilePathReader.readFile(path)

}
