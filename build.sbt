name := "marge"

organization := "mikiobraun"

version := "0.1"

scalaVersion := "2.11.7"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies += "de.bwaldvogel" % "liblinear" % "1.8"
libraryDependencies += "org.apache.commons" % "commons-math" % "2.2"

libraryDependencies += "junit" % "junit" % "4.12" % Test

//publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
