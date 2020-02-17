name := "CSCI-400 Labs"

version := "1.0"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.8" % "test" withSources() withJavadoc(),
    "jline" % "jline" % "2.14.2",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
)

scalacOptions += "-deprecation"

parallelExecution in test := false
