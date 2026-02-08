
ThisBuild / version := "0.1.0"
ThisBuild / versionScheme := Some("semver-spec")
ThisBuild / scalaVersion := "3.3.7"
ThisBuild / publishTo := localStaging.value

val dottyCpsAsyncVersion = "1.3.0"

lazy val root = project.in(file("."))
  .aggregate(rlLogic.jvm, rlLogic.js, rlLogic.native)
  .settings(
    publishArtifact := false,
    publish / skip := true,
  )

lazy val rlLogic = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name := "rl-logic",
    libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async" % dottyCpsAsyncVersion,
    libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async-logic" % dottyCpsAsyncVersion,
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.4" % Test,
  )
  .jvmSettings(
    libraryDependencies += "ai.djl" % "api" % "0.36.0",
    libraryDependencies += "ai.djl" % "bom" % "0.36.0",
    libraryDependencies += "ai.djl" % "model-zoo" % "0.36.0",
    libraryDependencies += "ai.djl.pytorch" % "pytorch-engine" % "0.36.0",
    // CUDA 12.4 enabled PyTorch native library for GPU support (requires CUDA toolkit)
    libraryDependencies += "ai.djl.pytorch" % "pytorch-native-cu124" % "2.5.1" % Runtime classifier "linux-x86_64",

    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.7",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18",
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
  ).nativeSettings(
  )
