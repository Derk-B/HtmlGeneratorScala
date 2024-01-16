val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala 3 Project Template",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.1.7",
    libraryDependencies += "io.circe" %% "circe-core" % "0.14.6",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.14.6",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.14.6"

  )
