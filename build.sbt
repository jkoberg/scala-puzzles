
name := "scala-puzzles"

scalaVersion in ThisBuild := "3.0.0-RC2"

scalacOptions in ThisBuild ++= Seq("-deprecation")

version in ThisBuild := "0.1.0"


//lazy val parser_puzzle =
//  project
//    .in(file("parser_puzzle"))
//    .settings(commonSettings)
//    .settings(scalaVersion := "2.13.5")


lazy val parser_puzzle_scala3 =
  project.in(file("parser_puzzle_scala3"))
