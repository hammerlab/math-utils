
lazy val base = rootProject(math)

lazy val math = project.settings(
  name := "math",
  version := "1.0.0",
  deps ++= Seq(
    cats,
    commons_math,
    iterators % "1.3.0",
    shapeless,
    spire
  ),
  testDeps += kryo
)

addScala212
