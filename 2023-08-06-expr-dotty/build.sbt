ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

val modules = file("modules")

lazy val langCore = project
  .in(modules / "lang-core")

lazy val lang = project
  .in(modules / "lang")
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"  %% "cats-parse"       % Version.catsParse,
      "org.typelevel"  %% "kittens"          % Version.kittens,
      "org.scala-lang" %% "scala3-compiler"  % Version.scalaLib,
      "org.scala-lang" %% "scala3-library"   % Version.scalaLib,
      "io.circe"       %% "circe-core"       % Version.circe,
      "io.circe"       %% "circe-generic"    % Version.circe,
      "org.scalameta"  %% "munit"            % Version.munit % Test
    )
  )
  .dependsOn(langCore)

lazy val root = (project in file("."))
  .aggregate(lang, langCore)
  .settings(
    name := "expr-dotty-05-08-2023"
  )
