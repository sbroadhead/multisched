import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "com.github.sbroadhead",
    version := "1.0.0",
    scalaVersion := "2.11.6",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("snapshots"),
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("public"),
      "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
    ),
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.2.2",
      "org.specs2" %% "specs2-core" % "3.6.1" % "test"
    ),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )
}

object MultiSchedBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in core
    )
  ) aggregate(macros, core)


  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _)
    )
  )

  lazy val core: Project = Project(
    "core",
    file("core"),
    settings = buildSettings ++ Seq(
      name := "multisched",
      libraryDependencies ++= Seq("org.reflections" % "reflections" % "0.9.10")
    )
  ) dependsOn macros
}
