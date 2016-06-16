organization := "edu.berkeley.eecs"

version := "0.1"

scalaVersion in ThisBuild := "2.11.7"

//val chiselVersion = "88293703119ed6468235bf240b16c637999add61"
val chiselVersion = "8292aee94408a0ccb583e0ce2c1ccedc81c39a1b"

lazy val chisel = ProjectRef(
  uri("git://github.com/ucb-bar/chisel.git#%s".format(chiselVersion)),
  "chisel"
)

val prjSettings = Project.defaultSettings ++ Seq(
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls",
                        "-language:implicitConversions", "-language:existentials"),
  libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0",
  libraryDependencies  ++= Seq(
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scalanlp" %% "breeze-viz" % "0.12"
  ),
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ ) 
)

lazy val ChiselCompatibility = Project(
  id = "chisel-compatibility", 
  base = file("ChiselCompatibility"),
  settings = prjSettings
).dependsOn(chisel)

lazy val root = Project(
  id = "chisel-dsp",
  base = file("."),
  settings = prjSettings
).dependsOn(ChiselCompatibility
).aggregate(ChiselCompatibility)
