ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.13.14"
ThisBuild / organization := "org.example"


val spinalVersion = "1.11.0"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalSim = "com.github.spinalhdl" %% "spinalhdl-sim" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

resolvers ++= Resolver.sonatypeOssRepos("snapshots") // For snapshots

lazy val fpu = (project in file("."))
  .settings(
    name := "fpu",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalSim, spinalIdslPlugin),
    Compile / scalaSource := baseDirectory.value / "hw" / "spinal",
    Test / scalaSource := baseDirectory.value / "hw" / "test"
  )

fork := true