ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.9"

lazy val root = (project in file("."))
  .settings(
    name := "SIDC",
    assemblyJarName in assembly := "SIDC.jar",
    mainClass in assembly := Some("Main"),
    unmanagedJars in Compile += file("/usr/local/Wolfram/WolframEngine/13.1/SystemFiles/Links/JLink/JLink.jar"),
  )

