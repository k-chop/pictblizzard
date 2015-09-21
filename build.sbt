
name := "pictblizzard"

version := "0.0.1"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "com.opencsv" % "opencsv" % "3.5",
  "com.typesafe.akka" %% "akka-actor" % "2.3.4",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "org.slf4j" % "slf4j-nop" % "1.7.12",
  "com.github.nscala-time" %% "nscala-time" % "2.2.0",
  "org.json4s" %% "json4s-native" % "3.3.0.RC6",
  "org.scalaz" %% "scalaz-core" % "7.1.4"
)

initialCommands in console := "import com.github.whelmaze.pictbliz; import pictbliz._; import pictbliz.scriptops._; import pictbliz.scriptops.Attrs._"

initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile

scalacOptions ++= Seq("-feature", "-unchecked", "-Xlint", "-language:_")

// add SWT to the unmanaged classpath
//unmanagedJars in Compile += file("/usr/share/java/swt.jar")

