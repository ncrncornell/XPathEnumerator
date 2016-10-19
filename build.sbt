
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
      "com.chuusai" %% "shapeless" % "2.3.2",
      "org.scalaz" %% "scalaz-core" % "7.2.6",
      "org.specs2" %% "specs2-core" % "3.8.5" % "it,test",
      "net.sf.saxon" % "Saxon-HE" % "9.7.0-8" % "it,test"
    )//,
    // other settings here
    //Note: Wart.Nothing is good in principle but too many false positives IMO
    // TODO: move Vars from warnings to errors
    ,wartremoverWarnings += Wart.Var
    //,wartremoverErrors ++= Warts.unsafe
    ,wartremoverErrors ++= Warts.allBut(Wart.Var, Wart.Nothing)
  )