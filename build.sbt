
name := "fixme"

version := "1.0"

scalaVersion := "2.12.19"



libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "3.5.3",
  "org.apache.spark" %% "spark-sql" % "3.5.3",
  "org.apache.spark" %% "spark-streaming" % "3.5.3",
  "org.apache.logging.log4j" % "log4j-core" % "2.24.2"
)


//Set spark logging level
fork / run := true
javaOptions / run := Seq(
  "-Dlog4j.debug=true",
  "-Dlog4j.configuration=log4j.properties")
outputStrategy := Some(StdoutOutput)


val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1"
scalacOptions += "-feature"
