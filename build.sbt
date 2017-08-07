
lazy val base =
  project
    .in(file("."))
    .settings(
      publish := {},
      test := {},
      publishArtifact := false
    )
    .aggregate(math)

lazy val math = project.settings(
  name := "math",
  version := "1.0.0",
  testUtilsVersion := "1.3.0",
  deps ++= Seq(
    cats,
    commons_math,
    iterators % "1.3.0",
    shapeless,
    spire
  ),
  testDeps += kryo
)
