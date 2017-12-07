lazy val scalaPureFunctional = (project in file("."))
  .settings(
    name := "scala-pure-functional",
    inThisBuild(List(
      organization := "somedotcom",
      scalaVersion := "2.12.3",
      version := "1.0-SNAPSHOT"
    )),
    fork in run := true,
    cancelable in Global := true

  ).aggregate(cats)

lazy val cats = (project in file("cats"))
  .settings(
    name := "cats",
    scalacOptions ++= Seq(
      "-Ypartial-unification"
    )
  )