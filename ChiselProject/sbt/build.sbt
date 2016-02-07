scalaVersion in ThisBuild := "2.11.7"

val prjSettings = Project.defaultSettings ++ Seq(
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls",
                        "-language:implicitConversions", "-language:existentials"),
  libraryDependencies += "org.json4s" %% "json4s-native" % "3.3.0",
  libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release",
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ ) 
)
 
lazy val ChiselCompatibility = Project(
  id = "chisel-compatibility", 
  base = file("ChiselCompatibility"),
  settings = prjSettings
)

lazy val ChiselDSP_Overlay = Project(
  id = "chisel-dsp-overlay",
  base = file("ChiselDSP_Overlay"),
  settings = prjSettings
).dependsOn(ChiselCompatibility)

lazy val ChiselDSP_Modules = Project(
  id = "chisel-dsp-modules",
  base = file("ChiselDSP_Modules"),
  settings = prjSettings
).dependsOn(ChiselDSP_Overlay)

lazy val root = Project(
  id = "chisel-project",
  base = file("."),
  settings = prjSettings
).dependsOn(ChiselDSP_Overlay,ChiselDSP_Modules
).aggregate(ChiselDSP_Overlay,ChiselDSP_Modules)
