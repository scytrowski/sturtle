name := "sturtle"

version := "0.1"

scalaVersion := "2.13.3"

// https://mvnrepository.com/artifact/org.typelevel/cats-core
libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0"

// https://mvnrepository.com/artifact/org.typelevel/cats-effect
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0"

// https://mvnrepository.com/artifact/org.scalatest/scalatest
libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test

lazy val geometry = (project in file("geometry"))
  .settings(
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test
  )

lazy val graphics = (project in file("graphics"))
  .settings(
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test
  )

lazy val es = (project in file("es"))
  .settings(
    // https://mvnrepository.com/artifact/org.typelevel/cats-core
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0",
    // https://mvnrepository.com/artifact/org.typelevel/cats-effect
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0",
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test
  )

lazy val core = (project in file("core"))
  .settings(
    // https://mvnrepository.com/artifact/org.typelevel/cats-core
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0",
    // https://mvnrepository.com/artifact/org.typelevel/cats-effect
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0",
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test,
    scalacOptions += "-Ypartial-unification"
  )
  .dependsOn(geometry, graphics, es)