import InstallCommand._

// Registering dependencies.
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"
libraryDependencies += "org.scalactic" %% "scalactic" % "2.2.6"
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "org.scala-lang" % "jline" % "2.9.0-1"

// Configuring the project.
lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  version := "0.0.0",

  commands ++= Seq(install, uninstall)
)

val jarName = "clasp.jar"
val dirName = "target"

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "clasp",
    assemblyJarName in assembly := jarName,
    assemblyOutputPath in assembly := file(s"$dirName/$jarName")
  )
