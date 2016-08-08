name := "logoot"

version := "3.0"

scalaVersion := "2.11.8"


libraryDependencies ++= Seq(
  "org.scalaz"     %% "scalaz-core"          % "7.2.4",
  "org.scalacheck" %% "scalacheck"           % "1.12.5" % "test",
  "org.specs2"     %% "specs2-core"          % "3.7"    % "test",
  "org.specs2"     %% "specs2-scalacheck"    % "3.7"    % "test",
  "org.specs2"     %% "specs2-matcher-extra" % "3.7"    % "test"
)

