ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "SIDC",
    assemblyJarName in assembly := "SIDC.jar",
    mainClass in assembly := Some("Main"),
    // Add dependency on ScalaFX library
    libraryDependencies += "org.scalafx" %% "scalafx" % "19.0.0-R30",

    scalacOptions ++= Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-feature"),

    unmanagedJars in Compile += file("/usr/local/Wolfram/WolframEngine/13.1/SystemFiles/Links/JLink/JLink.jar"),
  )

