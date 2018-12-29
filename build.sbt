
default(
  group("org.hammerlab.math"),
  versions(
     io_utils → "5.1.1",
    iterators → "2.1.0"
  )
)

lazy val cubic = cross.settings(
  v"1.0.1",
  dep(
    io_utils,
    iterators,
    shapeless,
    spire
  ),
  publishTestJar
).dependsOn(
  tolerance,
      utils,
     format,
      types test
)
lazy val cubicX = cubic.x

lazy val format = cross.settings(
  v"1.1.1",
  dep(cats)
)
lazy val formatX = format.x

lazy val quartic = cross.settings(
  v"1.0.1",
  dep(
    cats,
    io_utils,
    shapeless,
    spire,
    iterators.tests
  ),
  publishTestJar
).dependsOn(
     format,
  tolerance,
      utils,
      cubic andTest,
      types test
)
lazy val quarticX = quartic.x

lazy val stats = cross.settings(
  v"1.3.3",
  dep(
    cats,
    io_utils,
    iterators,
    spire
  )
).dependsOn(
  format,
  tolerance,
  types,
  utils
)
lazy val statsX = stats.x

lazy val tolerance = cross.settings(
  v"1.0.1",
  dep(cats),
  // test-utils depends on this module for fuzzy-equality / tolerant-double comparisons, and dependency-resolvers
  // emit circular-dependency false-positives when `a` depends on `b` and `b` depends on `a`'s tests
  testDeps := Seq(scalatest),
  publishTestJar
)
lazy val toleranceX = tolerance.x

lazy val types =
  cross
    .settings(
      group("org.hammerlab"),
      v"1.4.0",
      dep(
        cats,
        shapeless,
        spire
      )
    )
    .jvmSettings(
      http4s.version := "0.18.13",
      dep(
        // UrlTest runs a dummy server
        http4s. dsl           tests,
        http4s.`blaze-server` tests
      )
    )
lazy val typesX = types.x

lazy val utils =
  cross
    .settings(
      v"2.3.0",
      dep(
        cats,
        shapeless,
        spire
      ),
      consolePkg("hammerlab.math")
    )
    .jvmSettings(
      // a couple components depend on Java-only libraries and are only included in the JVM module
      dep(
        commons.math,
        kryo tests
      )
    )
lazy val utilsX = utils.x

lazy val `math-utils` =
  root(
        cubicX,
       formatX,
      quarticX,
        statsX,
    toleranceX,
        typesX,
        utilsX
  )
