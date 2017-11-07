
lazy val math = project.settings(
  name := "math",
  version := "2.1.0",
  deps ++= Seq(
    cats,
    commons_math,
    iterators % "2.0.0",
    shapeless,
    spire
  ),
  testDeps += kryo
)

lazy val stats = project.settings(
  name := "stats",
  version := "1.1.1",
  deps ++= Seq(
    cats,
    io % "3.0.0",
    iterators % "2.0.0",
    spire
  )
).dependsOn(
  math,
  types
)

lazy val types = project.settings(
  name := "types",
  version := "1.0.1",
  deps ++= Seq(
    cats,
    shapeless,
    spire
  )
)

addScala212

lazy val base = rootProject(math, stats, types)
