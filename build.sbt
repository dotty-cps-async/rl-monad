

val dottyCpsAsyncVersion = "1.0.0"


lazy val rlLogic = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file(".")) 
  .settings(
    scalaVersion := "3.3.5",
    name := "rl-logic",
    libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async" % dottyCpsAsyncVersion,
    libraryDependencies += "io.github.dotty-cps-async" %%% "dotty-cps-async-logic" % dottyCpsAsyncVersion,
    libraryDependencies += "org.scalameta" %%% "munit" % "1.0.4" % Test,
  ).jsSettings(
    scalaJSUseMainModuleInitializer := true,
  ).nativeSettings(
  )
