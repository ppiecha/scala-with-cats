lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "3.5.2"
    )),
    name := "scala-with-cats"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.scalacheck" %% "scalacheck" % "1.19.0"
)
