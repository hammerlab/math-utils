
lazy val math = project.settings(
  version := "2.1.2-SNAPSHOT",
  deps ++= Seq(
    cats,
    commons_math,
    iterators % "2.0.0",
    shapeless,
    spire
  ),
  testDeps += kryo,
  initialCommands := "import hammerlab.math._"
)

lazy val stats = project.settings(
  version := "1.1.2",
  deps ++= Seq(
    cats,
    io % "3.1.0",
    iterators % "2.0.0",
    spire
  )
).dependsOn(
  math,
  types
)

lazy val types = project.settings(
  version := "1.0.1",
  deps ++= Seq(
    cats,
    shapeless,
    spire
  )
)

addScala212

lazy val base = rootProject("math-base", math, stats, types)
