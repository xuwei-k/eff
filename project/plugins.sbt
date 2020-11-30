addSbtPlugin("com.jsuereth"         % "sbt-pgp"               % "2.1.1")
addSbtPlugin("com.typesafe.sbt"     % "sbt-ghpages"           % "0.6.3")
addSbtPlugin("com.typesafe.sbt"     % "sbt-site"              % "1.4.1")
addSbtPlugin("org.scoverage"        % "sbt-scoverage"         % "1.6.0")
addSbtPlugin("com.typesafe.sbt"     % "sbt-git"               % "1.0.0")
addSbtPlugin("org.scala-js"         % "sbt-scalajs"           % "1.3.1")
addSbtPlugin("org.portable-scala"   % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.xerial.sbt"       % "sbt-sonatype"          % "3.9.5")
addSbtPlugin("ohnosequences"        % "sbt-github-release"    % "0.7.0")
addSbtPlugin("com.eed3si9n"         % "sbt-buildinfo"         % "0.10.0")
addSbtPlugin("ch.epfl.lamp"         % "sbt-dotty"             % "0.4.6")

// https://github.com/ohnosequences/sbt-github-release/issues/28#issuecomment-426086656
libraryDependencies += "com.sun.activation" % "javax.activation" % "1.2.0"
