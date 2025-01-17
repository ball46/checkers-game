ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.4"

val http4sVersion = "0.23.30"
val circeVersion = "0.14.10"
val catsEffectVersion = "3.5.7"

lazy val root = (project in file("."))
  .settings(
    name := "scala-checkers",
    
    libraryDependencies ++= Seq(
      // HTTP4S
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,

      // Circe for JSON
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,

      // Cats Effect
      "org.typelevel" %% "cats-effect" % catsEffectVersion,

      // Testing
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.typelevel" %% "cats-effect-testing-scalatest" % "1.6.0" % Test,

      "ch.qos.logback" % "logback-classic" % "1.5.16",
      "org.slf4j" % "slf4j-api" % "2.0.16"
    ),

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-explain",
      "-explain-types",
      "-language:higherKinds",
      "-Xmax-inlines", "64"
    )
  )