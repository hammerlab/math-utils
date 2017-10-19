
lazy val base = rootProject(math, stats)

lazy val math = project.settings(
  name := "math",
  version := "2.0.0",
  deps ++= Seq(
    cats,
    commons_math,
    iterators % "1.3.0",
    shapeless,
    spire
  ),
  testDeps += kryo
)

lazy val stats = project.settings(
  name := "stats",
  version := "1.1.0",
  deps ++= Seq(
    cats,
    io % "2.0.0",
    iterators % "1.3.0",
    spire
  )
).dependsOn(
  math
)

addScala212
