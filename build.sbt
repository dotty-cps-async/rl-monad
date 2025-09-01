

val dottyCpsAsyncVersion = "1.1.3-SNAPSHOT"


lazy val rlLogic = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    scalaVersion := "3.3.6",
    name := "rl-logic",
    libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async" % dottyCpsAsyncVersion,
    libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async-logic" % dottyCpsAsyncVersion,
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.4" % Test,
  )
  .jvmSettings(
    libraryDependencies += "ai.djl" % "api" % "0.32.0",
    libraryDependencies += "ai.djl" % "bom" % "0.32.0",
    libraryDependencies += "ai.djl" % "model-zoo" % "0.32.0",
    libraryDependencies += "ai.djl.pytorch" % "pytorch-engine" % "0.32.0",

    libraryDependencies += "org.slf4j" % "slf4j-api" % "2.0.7",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18",
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
  ).nativeSettings(
  )
