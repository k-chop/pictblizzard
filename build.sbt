name := "pictblizzard"

version := "0.0.1"

organization := "com.github.chuwb"

scalaVersion := "2.9.2"

//parallelExecution in Test := false

resolvers ++= Seq("twitter" at "http://maven.twttr.com/",
                  //"Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
                  "Sonatype OSS releases" at "https://oss.sonatype.org/content/repositories/releases")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.8" % "test",
  "com.twitter" % "util-logging" % "5.3.12",
  "net.sf.opencsv" % "opencsv" % "2.0"
  //"swt" % "jface" % "3.0.1"
)

// reduce the maximum number of errors shown by the Scala compiler
maxErrors := 20

// increase the time between polling for file changes when using continuous execution
pollInterval := 1000

scalacOptions ++= Seq("-deprecation", "-unchecked")

// set the prompt (for this build) to include the project id.
//shellPrompt in ThisBuild := { state => Project.extract(state).currentRef.project + "> " }

// disable printing timing information, but still print [success]
showTiming := true

// disable printing a message indicating the success or failure of running a task
showSuccess := true

// only show 10 lines of stack traces
traceLevel := 10

// add SWT to the unmanaged classpath
//unmanagedJars in Compile += file("/usr/share/java/swt.jar")

