lazy val root = project
  .in(file("."))
  .settings(
    name := "algorithms",
    description := "place to explore algorithmic adventures in Scala 3",
    version := "0.1.0",
    scalaVersion := "3.6.2",
    scalacOptions ++= Seq(),
    ThisBuild/scalacOptions ++= Seq(
      "-unchecked", 
      "-deprecation", 
      "-Ykind-projector:underscores" // to allow C{*}/C[_] as a synonym for [A] =>> C[A]
    ),
    libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.0.0-M3" % Test,
        "org.typelevel" % "cats-core_3" % "2.10.0",
        "org.typelevel" % "cats-free_3" % "2.10.0"
    )
  )

testFrameworks += new TestFramework("munit.Framework")
