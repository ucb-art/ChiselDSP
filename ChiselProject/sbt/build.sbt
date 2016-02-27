scalaVersion in ThisBuild := "2.11.7"

val chiselVersion = "88293703119ed6468235bf240b16c637999add61"

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
