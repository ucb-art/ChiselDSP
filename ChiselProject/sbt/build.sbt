addSbtPlugin("com.github.scct" % "sbt-scct" % "0.2.1")

lazy val ChiselCompatibility = Project(
  id = "chisel-compatibility", 
  base = file("ChiselCompatibility"),
  settings = Project.defaultSettings ++ Seq(
    name := "chisel-compatibility",
    version := "1.0",
    scalaVersion := "2.10.6",
    libraryDependencies += "edu.berkeley.cs" %% "chisel" % "2.2.27",
    libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _ )
  )
)
lazy val ChiselDSP_Overlay = project.dependsOn(ChiselCompatibility)
lazy val ChiselDSP_Modules = project.dependsOn(ChiselDSP_Overlay)
lazy val root = project.in(file(".")).dependsOn(ChiselDSP_Overlay,ChiselDSP_Modules)
