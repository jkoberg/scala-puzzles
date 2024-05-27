
name := "scala-example"

val scala3Version = "3.3.3"
val scala2Version = "2.13.13"
val akkaVersion = "2.7.0"
val akkaHttpVersion = "10.4.0"
val playJsonVersion = "2.9.3"

ThisBuild / scalacOptions ++= Seq("-deprecation")
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := scala3Version

val akkaDeps = Seq(
  ("com.typesafe.akka" %% "akka-actor-typed" % akkaVersion).cross(CrossVersion.for3Use2_13),
  ("com.typesafe.akka" %% "akka-http" % akkaHttpVersion).cross(CrossVersion.for3Use2_13),
  ("com.typesafe.akka" %% "akka-stream" % akkaVersion).cross(CrossVersion.for3Use2_13),
  ("com.typesafe.play" %% "play-json" % playJsonVersion).cross(CrossVersion.for3Use2_13)
)

lazy val sensor_stream = project
  .in(file("sensor_stream"))
  .settings(
    scalaVersion := scala2Version,
    libraryDependencies ++= akkaDeps
  )

lazy val wordwrap = project
  .in(file("wordwrap"))


lazy val digraph = project
  .in(file("digraph"))


lazy val first_unique_int = project
  .in(file("first_unique_int"))


lazy val merge_ranges = project
  .in(file("merge_ranges"))

lazy val programming_language_parser = project
  .in(file("programming_language_parser"))


lazy val pagerduty_cli = project
  .in(file("pagerduty_cli"))
  .settings(
    scalaVersion := scala2Version,
    libraryDependencies ++= akkaDeps
  )

lazy val worksheet_test = project
  .in(file("worksheet_test"))

lazy val parser_example = project
  .in(file("parser_example"))


lazy val file_search_api = project
  .in(file("file_search_api"))


lazy val k_nearest_points = project
  .in(file("k_nearest_points"))


lazy val multivalue_dictionary = project
  .in(file("multivalue_dictionary"))

lazy val leetcode = project
  .in(file("leetcode"))

lazy val in_memory_database = project
  .in(file("in_memory_database"))

lazy val trees = project
  .in(file("trees"))