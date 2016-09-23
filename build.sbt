
lazy val commonSettings = Seq(
  name := "XPathEnumerator",
  version := "1.0",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  configs(IntegrationTest).
  settings(commonSettings: _*).
  settings(Defaults.itSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      //"org.scalaxb" % "scalaxb_2.11" % "1.4.1",
      // "co.fs2" %% "fs2-core" % "0.9.0-RC2",
      "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
      "org.specs2" %% "specs2-core" % "3.8.5" % "it,test"
    )
    // other settings here
  )