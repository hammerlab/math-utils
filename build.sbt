
lazy val math = project.settings(
  name := "math",
  version := "2.1.0-SNAPSHOT",
  deps ++= Seq(
    cats,
    commons_math,
    iterators % "1.4.0",
    shapeless,
    spire
  ),
  testDeps += kryo
)

lazy val monoids = project.settings(
  name := "monoids",
  version := "1.0.0-SNAPSHOT",
  deps ++= Seq(
    cats,
    shapeless,
    spire
  )
)

lazy val stats = project.settings(
  name := "stats",
  version := "1.1.1-SNAPSHOT",
  deps ++= Seq(
    cats,
    io % "3.0.0-SNAPSHOT",
    iterators % "1.4.0",
    spire
  )
).dependsOn(
  math,
  monoids
)

addScala212

lazy val base = rootProject(math, monoids, stats)
