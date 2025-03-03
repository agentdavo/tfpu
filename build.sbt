ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.12.18"
ThisBuild / organization := "org.example"

val spinalVersion = "1.11.0"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

lazy val fpu = (project in file("."))
  .settings(
  
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin),
	name := "fpu",
	Compile / scalaSource  := baseDirectory.value / "hw" / "spinal",
    Test / scalaSource     := baseDirectory.value / "hw" / "test"
  )

fork := true