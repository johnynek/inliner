// Versions:
val macroParadiseV = "2.0.1"
val scalacheckV = "1.12.4"
val scalatestV = "3.0.0-M7"


lazy val commonSettings = Seq(
  organization := "com.github.johnynek",
  name := "inliner",
  version := "0.0.1",
  scalaVersion := "2.11.7",
  crossScalaVersions := Seq("2.10.5", "2.11.7"),
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-unchecked",
    "-Xlint",
    "-Yno-adapted-args",
    "-Ywarn-dead-code",
    "-Ywarn-numeric-widen",
    //"-Ywarn-value-discard", // fails with @sp on Unit
    "-Xfuture"
  ),
  libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % scalacheckV % "test",
      "org.scalatest" %% "scalatest" % scalatestV % "test"))

lazy val crossVersionSharedSources: Seq[Setting[_]] =
  Seq(Compile, Test).map { sc =>
    (unmanagedSourceDirectories in sc) ++= {
      (unmanagedSourceDirectories in sc ).value.map {
        dir:File => new File(dir.getPath + "_" + scalaBinaryVersion.value)
      }
    }
  }

lazy val core = Project(id = "core",
  base = file("core"),
  settings = commonSettings ++ Seq(name := "core"))
  .settings(crossVersionSharedSources:_*)
  .settings(
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
    libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq()
        // in Scala 2.10, quasiquotes are provided by macro paradise
        case Some((2, 10)) =>
          Seq(
            compilerPlugin("org.scalamacros" % "paradise" % macroParadiseV cross CrossVersion.full),
            "org.scalamacros" %% "quasiquotes" % macroParadiseV cross CrossVersion.binary
          )
      })
  )
