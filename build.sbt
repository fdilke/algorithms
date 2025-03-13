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
      "-Xkind-projector:underscores" // to allow C{*}/C[_] as a synonym for [A] =>> C[A]
    ),
    libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.1.0" % Test,
        "org.typelevel" % "cats-core_3" % "2.13.0",
        "org.typelevel" % "cats-free_3" % "2.13.0",
        "org.redfx" % "strangefx" % "0.1.4"
    )
  )

testFrameworks += new TestFramework("munit.Framework")
