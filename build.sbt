lazy val root = project
  .in(file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name         := "adventofcode2021",
    description  := "Jasper's advent of code stuff for 2021",
    version      := "0.1.0",
    scalaVersion := "3.1.0",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test
    ),
    Compile / mainClass := Some("eu.derfniw.aoc2021.d13.run_13_2"),
    nativeImageOptions ++= Seq(
      "--static"
    )
  )
