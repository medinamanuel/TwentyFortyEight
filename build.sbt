name := "Game2048"

version := "0.1"

scalaVersion := "2.10.3"

organization := "org.mmg.tools.json"

resolvers ++= Seq(
    "RoundEights" at "http://maven.spikemark.net/roundeights"
)


libraryDependencies ++= Seq(
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3",
  "org.scalaz" %% "scalaz-core" % "7.1.0"
)
