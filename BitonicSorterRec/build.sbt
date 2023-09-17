scalaVersion := "2.13.10"
val chiselVersion = "3.5.6"
val chiselTestVersion = "0.5.6"


lazy val root = (project in file("."))
  .settings(
    name := "BitonicSorterRec"
  )

addCompilerPlugin(("edu.berkeley.cs" %% "chisel3-plugin" % chiselVersion).cross(CrossVersion.full))
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % chiselVersion
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % chiselTestVersion

