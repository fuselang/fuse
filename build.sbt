import scala.scalanative.build._

// Define common settings for all platforms
val sharedSettings = Seq(
  scalaVersion := "3.6.4",
  name := "fuse",
  version := "0.2.0", // x-release-please-version
  scalacOptions ++= Seq(
    "-feature",
    "-rewrite",
    "-source:future-migration",
    "-deprecation",
    "-unchecked",
    "-language:postfixOps"
  ),
  libraryDependencies += "org.parboiled" %%% "parboiled" % "2.5.0",
  libraryDependencies += "org.typelevel" %%% "cats-core" % "2.11.0",
  libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.5.7" withSources () withJavadoc (),
  libraryDependencies += "com.monovore" %%% "decline" % "2.4.0",
  libraryDependencies += "com.monovore" %%% "decline-effect" % "2.4.0",
  libraryDependencies += "com.lihaoyi" %%% "fansi" % "0.4.0",
  libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0-M11" % Test,
  libraryDependencies += "org.typelevel" %%% "munit-cats-effect" % "2.0.0-M5" % Test,
  testFrameworks += new TestFramework("munit.Framework")
)

// Scala Native release configuration
val nativeSettings = Seq(
  nativeConfig ~= { config =>
    val ltoMode =
      if (System.getProperty("os.name").toLowerCase.contains("mac")) LTO.none
      else LTO.thin
    config
      .withLTO(ltoMode)
      .withMode(Mode.releaseFast)
      .withGC(GC.immix)
  }
)

// Define a cross project for JVM and Native platforms
lazy val fuse = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(sharedSettings)
  .nativeSettings(nativeSettings)

lazy val fuseJVM = fuse.jvm
lazy val fuseNative = fuse.native

lazy val root =
  (project in file(".")).aggregate(fuseJVM, fuseNative).settings(sharedSettings)
