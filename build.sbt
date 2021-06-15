
name := "scala-example"


ThisBuild / scalacOptions ++= Seq("-deprecation")
ThisBuild / version := "0.1.0"


val scala2Version = "2.13.6"

val akkaVersion = "2.6.14"
val akkaHttpVersion = "10.2.4"

lazy val sensor_stream = project
  .in(file("sensor_stream"))
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
    )
  )

lazy val wordwrap = project
  .in(file("wordwrap"))
  .settings(scalaVersion  := scala2Version)


lazy val digraph = project
  .in(file("digraph"))
  .settings(scalaVersion  := scala2Version)


lazy val first_unique_int = project
  .in(file("first_unique_int"))
  .settings(scalaVersion  := scala2Version)


lazy val merge_ranges = project
  .in(file("merge_ranges"))
  .settings(scalaVersion  := scala2Version)

lazy val programming_language_parser = project
  .in(file("programming_language_parser"))
  .settings(scalaVersion := scala2Version)


lazy val pagerduty_cli = project
  .in(file("pagerduty_cli"))
  .settings(scalaVersion  := scala2Version,
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.play" %% "play-json" % "2.9.2"
    )
  )



lazy val worksheet_test = project
  .in(file("worksheet_test"))
  .settings(scalaVersion := scala2Version)