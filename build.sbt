
lazy val math = project.settings(
  r"2.1.2",
  dep(
    cats,
    commons_math,
    iterators % "2.0.0",
    kryo tests,
    shapeless,
    spire
  ),
  consolePkg("hammerlab.math")
)

lazy val stats = project.settings(
  r"1.2.0",
  dep(
         cats,
     io_utils % "4.0.0",
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

lazy val base = rootProject("math-utils", math, stats, types)
