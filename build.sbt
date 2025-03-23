// Define common settings for all platforms
val sharedSettings = Seq(
  scalaVersion := "3.6.4",
  name := "fuse",
  version := "0.1",
  scalacOptions ++= Seq(
    "-feature",
    "-rewrite",
    "-source:future-migration",
    "-deprecation",
    "-unchecked",
    "-language:postfixOps"
  ),
  libraryDependencies += "org.parboiled" %%% "parboiled" % "2.5.0",
  libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
  libraryDependencies += "org.typelevel" %%% "cats-effect" % "3.5.7" withSources () withJavadoc (),
  libraryDependencies += "com.monovore" %%% "decline" % "2.5.0",
  libraryDependencies += "com.monovore" %%% "decline-effect" % "2.5.0",
  libraryDependencies += "com.lihaoyi" %%% "fansi" % "0.4.0",
  libraryDependencies += "org.scalameta" %%% "munit" % "1.0.4" % Test,
  testFrameworks += new TestFramework("munit.Framework")
)

// Define a cross project for JVM and Native platforms
lazy val fuse = crossProject(JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("."))
  .settings(sharedSettings)

lazy val fuseJVM = fuse.jvm

lazy val root =
  (project in file(".")).aggregate(fuseJVM).settings(sharedSettings)
