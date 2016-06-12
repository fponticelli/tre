version := "1.0"
scalaVersion in ThisBuild := "2.11.8"

resolvers += sbt.Resolver.bintrayRepo("denigma", "denigma-releases") //add resolver

lazy val root = project.in(file(".")).
  aggregate(treJS, treJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val tre = crossProject.in(file(".")).
  settings(
    name := "tre",
    version := "0.1-SNAPSHOT"
  ).
  jvmSettings(
    // Add JVM-specific settings here
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies += "org.denigma" %%% "threejs-facade" % "0.0.77-SNAPSHOT" //add dependency
  )

lazy val treJVM = tre.jvm
lazy val treJS = tre.js
