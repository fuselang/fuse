import scala.scalanative.build._

// Define common settings for all platforms
val sharedSettings = Seq(
  scalaVersion := "3.6.4",
  name := "fuse",
  version := "0.4.0", // x-release-please-version
  scalacOptions ++= Seq(
    "-feature",
    "-rewrite",
    "-source:future-migration",
    "-deprecation",
    "-unchecked",
    "-language:postfixOps"
  ),
  resolvers += "jitpack" at "https://jitpack.io",
  libraryDependencies += "com.github.sirthias.parboiled2" %%% "parboiled" % "6c4471fc8b",
  libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
  libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.7.0" withSources () withJavadoc (),
  libraryDependencies += "com.monovore" %%% "decline" % "2.6.1",
  libraryDependencies += "com.monovore" %%% "decline-effect" % "2.6.1",
  libraryDependencies += "com.lihaoyi" %%% "fansi" % "0.5.1",
  libraryDependencies += "org.scalameta" %%% "munit" % "1.2.4" % Test,
  libraryDependencies += "org.typelevel" %%% "munit-cats-effect" % "2.2.0" % Test,
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
