name := "ajd"

organization := "com.github.mdr"

version := "1.0.0"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scala-lang"            % "scala-reflect"                     % scalaVersion.value,
  "org.json4s"               %% "json4s-jackson"                    % "3.5.2",
  "com.google.guava"          % "guava"                             % "23.0",
  "org.scalatest"            %% "scalatest"                         % "3.0.3" % Test)
