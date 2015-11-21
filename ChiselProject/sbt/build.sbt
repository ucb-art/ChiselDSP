scalaVersion in ThisBuild := "2.11.7"

lazy val ChiselCompatibility = Project(
  id = "chisel-compatibility", 
  base = file("ChiselCompatibility"),
  settings = Project.defaultSettings ++ Seq(
    name := "chisel-compatibility",
    version := "1.0",
    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "latest.release",
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )
  )
)
lazy val ChiselDSP_Overlay = project.dependsOn(ChiselCompatibility)
lazy val ChiselDSP_Modules = project.dependsOn(ChiselDSP_Overlay)
lazy val root = project.in(file(".")).dependsOn(ChiselDSP_Overlay,ChiselDSP_Modules)
