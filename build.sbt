lazy val root = project
  .in(file("."))
  .settings(
    name := "algorithms",
    description := "place to explore algorthmic adventures in Scala 3",
    version := "0.1.0",
    scalaVersion := "3.2.1",
    scalacOptions ++= Seq(),
    ThisBuild/scalacOptions ++= Seq("-unchecked", "-deprecation"),
    libraryDependencies ++= Seq(
        "org.scalameta" %% "munit" % "1.0.0-M3" % Test,
        "org.typelevel" % "cats-core_3" % "2.7.0"
    )
  )

testFrameworks += new TestFramework("munit.Framework")
