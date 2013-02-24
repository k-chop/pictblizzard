name := "pictblizzard"

version := "0.0.1"

organization := "com.github.chuwb"

scalaVersion := "2.10.0"

//parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "net.sf.opencsv" % "opencsv" % "2.0",
  "org.clapper" %% "grizzled-slf4j" % "1.0.1",
  "org.slf4j" % "slf4j-api" % "1.7.1",
  "ch.qos.logback" % "logback-classic" % "1.0.7"
)

initialCommands in console := "import com.github.whelmaze.pictbliz; import pictbliz._; import pictbliz.scriptops._; import pictbliz.scriptops.Attrs._"

initialCommands in (Compile, consoleQuick) <<= initialCommands in Compile

scalacOptions += "-feature"

fork in run := true

// reduce the maximum number of errors shown by the Scala compiler
maxErrors := 20

// increase the time between polling for file changes when using continuous execution
pollInterval := 1000

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

