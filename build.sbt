name := "sturtle"

version := "0.1"

val languageVersion = "2.13.3"

lazy val geometry = (project in file("geometry"))
  .settings(
    scalaVersion := languageVersion,
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test
  )

lazy val graphics = (project in file("graphics"))
  .settings(
    scalaVersion := languageVersion,
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test
  )

lazy val es = (project in file("es"))
  .settings(
    scalaVersion := "2.13.3",
    // https://mvnrepository.com/artifact/org.typelevel/cats-core
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0",
    // https://mvnrepository.com/artifact/org.typelevel/cats-effect
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0",
    // https://mvnrepository.com/artifact/io.chrisdavenport/log4cats-core
    libraryDependencies += "io.chrisdavenport" %% "log4cats-core" % "1.1.1",
    // https://mvnrepository.com/artifact/io.chrisdavenport/log4cats-slf4j
    libraryDependencies += "io.chrisdavenport" %% "log4cats-slf4j" % "1.1.1",
    // https://mvnrepository.com/artifact/org.slf4j/slf4j-log4j12
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.30" % Test,
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test
  )

lazy val core = (project in file("core"))
  .settings(
    scalaVersion := languageVersion,
    // https://mvnrepository.com/artifact/org.typelevel/cats-core
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0",
    // https://mvnrepository.com/artifact/org.typelevel/cats-effect
    libraryDependencies += "org.typelevel" %% "cats-effect" % "2.2.0",
    // https://mvnrepository.com/artifact/io.chrisdavenport/log4cats-core
    libraryDependencies += "io.chrisdavenport" %% "log4cats-core" % "1.1.1",
    // https://mvnrepository.com/artifact/io.chrisdavenport/log4cats-slf4j
    libraryDependencies += "io.chrisdavenport" %% "log4cats-slf4j" % "1.1.1",
    // https://mvnrepository.com/artifact/org.slf4j/slf4j-log4j12
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.30" % Test,
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test
  )
  .dependsOn(geometry, graphics, es)

lazy val remoting = (project in file("remoting"))
  .settings(
    scalaVersion := languageVersion,
    // https://mvnrepository.com/artifact/org.http4s/http4s-blaze-server
    libraryDependencies += "org.http4s" %% "http4s-blaze-server" % "0.21.7",
    // https://mvnrepository.com/artifact/org.http4s/http4s-dsl
    libraryDependencies += "org.http4s" %% "http4s-dsl" % "0.21.7",
    // https://mvnrepository.com/artifact/org.scodec/scodec-core
    libraryDependencies += "org.scodec" %% "scodec-core" % "1.11.7",
    // https://mvnrepository.com/artifact/org.scodec/scodec-bits
    libraryDependencies += "org.scodec" %% "scodec-bits" % "1.1.20",
    // https://mvnrepository.com/artifact/org.scodec/scodec-stream
    libraryDependencies += "org.scodec" %% "scodec-stream" % "2.0.0",
    // https://mvnrepository.com/artifact/io.chrisdavenport/log4cats-core
    libraryDependencies += "io.chrisdavenport" %% "log4cats-core" % "1.1.1",
    // https://mvnrepository.com/artifact/io.chrisdavenport/log4cats-slf4j
    libraryDependencies += "io.chrisdavenport" %% "log4cats-slf4j" % "1.1.1",
    // https://mvnrepository.com/artifact/org.slf4j/slf4j-log4j12
    libraryDependencies += "org.slf4j" % "slf4j-log4j12" % "1.7.30" % Test,
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.3.0-SNAP2" % Test,
  )
  .dependsOn(es, core)

lazy val persistence = (project in file("persistence"))
  .settings(
    scalaVersion := languageVersion
  )
  .dependsOn(es, core)