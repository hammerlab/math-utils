
lazy val math = project.settings(
  v"2.1.2",
  dep(
    cats,
    commons_math,
    iterators % "2.0.0",
    kryo tests,
    shapeless,
    spire
  ),
  consolePkgs += "hammerlab.math"
)

lazy val stats = project.settings(
  v"1.2.0",
  dep(
    cats,
    io % "4.0.0" snapshot,
    iterators % "2.0.0",
    spire
  )
).dependsOn(
  math,
  types
)

lazy val types = project.settings(
  r"1.0.1",
  dep(
    cats,
    shapeless,
    spire
  )
)

addScala212

lazy val base = rootProject("math-base", math, stats, types)
