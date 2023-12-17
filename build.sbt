ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "PlProject",

      libraryDependencies ++= Seq(
       "de.tu-darmstadt.stg" %% "rescala" % "0.33.0",
      "com.typesafe.akka" %% "akka-actor" % "2.8.0",
      "com.typesafe.akka" %% "akka-actor" % "2.8.0",
       "org.openjfx" % "javafx-controls" % "19.0.2.1",
       "org.openjfx" % "javafx-fxml" % "19.0.2.1",
      "com.typesafe.akka" %% "akka-actor" % "2.8.0",
      "org.openjfx" % "javafx-controls" % "19.0.2.1",
      "org.openjfx" % "javafx-fxml" % "19.0.2.1",
        "org.scalafx" %% "scalafx" % "8.0.192-R14",
      "org.scala-lang" % "scala-library" % scalaVersion.value


    )

  )
mainClass in (Compile, run) := Some("BankingGUI")