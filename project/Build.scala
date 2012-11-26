import sbt._
import Keys._

object Build extends Build {

  override def settings = super.settings ++ Seq(
    scalaVersion := "2.9.2",
    crossScalaVersions := Seq("2.9.0", "2.9.0-1", "2.9.1", "2.9.1-1", "2.9.2", "2.10.0-RC2", "2.10.0-RC3"),
    name := "stomatepia-mongo",
    organization := "no.arktekk")

  lazy val root = Project("stomatepia", file(".")).aggregate(api, mongoJavaDriver)

  lazy val api = Project("api", file("api")) settings(
    libraryDependencies <+= (scalaVersion){ v => "org.scalatest" %% "scalatest" % scalatest(v) % "test" cross CrossVersion.full })

  lazy val mongoJavaDriver = Project("mongo-java-driver", file("mongo-java-driver")) settings (
    libraryDependencies += "org.mongodb" % "mongo-java-driver" % "2.9.1") dependsOn(api)

  lazy val scalatest = Map("2.10.0-RC3" -> "1.8-B1").withDefaultValue("1.8")
}