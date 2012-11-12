import sbt._
import Keys._

object Build extends Build {

  override def settings = super.settings ++ Seq(
    scalaVersion := "2.9.2",
    name := "stomatepia-mongo",
    organization := "no.arktekk")

  lazy val root = Project("stomatepia", file(".")).aggregate(api, mongoJavaDriver)

  lazy val api = Project("api", file("api")) settings(
    libraryDependencies += "org.scalatest" %% "scalatest" % "1.8" % "test")

  lazy val mongoJavaDriver = Project("mongo-java-driver", file("mongo-java-driver")) settings (
    libraryDependencies += "org.mongodb" % "mongo-java-driver" % "2.9.1") dependsOn(api)
}