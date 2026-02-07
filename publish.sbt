credentials += Credentials(Path.userHome / ".sbt" / "central_sonatype_credentials")

ThisBuild / organization := "io.github.dotty-cps-async"
ThisBuild / organizationName := "dotty-cps-async"
ThisBuild / organizationHomepage := Some(url("https://github.com/dotty-cps-async"))

ThisBuild / scmInfo := Some(
       ScmInfo(
          url("https://github.com/rssh/rl-logic"),
          "scm:git@github.com:rssh/rl-logic.git"
       )
)

ThisBuild / developers := List(
          Developer(
             id    = "rssh",
             name  = "Ruslan Shevchenko",
             email = "ruslan@shevchenko.kiev.ua",
             url   = url("https://github.com/rssh")
          )
)

ThisBuild / description := "monad for reinforcement learning"
ThisBuild / licenses := List("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/rssh/rl-logic"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true
