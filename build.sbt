
default(
  group("org.hammerlab.math"),
  versions(
     io_utils → "5.2.1",
    iterators → "2.1.0"
  )
)

lazy val cubic = cross.settings(
  r"1.0.0",
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
lazy val `cubic-x` = cubic.x

lazy val format = cross.settings(
  r"1.1.1",
  dep(cats)
)
lazy val `format-x` = format.x

lazy val quartic = cross.settings(
  r"1.0.0",
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
lazy val `quartic-x` = quartic.x

lazy val stats = cross.settings(
  r"1.3.3",
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
lazy val `stats-x` = stats.x

lazy val  tolerance = cross.settings(
  r"1.0.1",
  dep(cats),
  // test-utils depends on this module for fuzzy-equality / tolerant-double comparisons, and dependency-resolvers
  // emit circular-dependency false-positives when `a` depends on `b` and `b` depends on `a`'s tests
  testDeps := Seq(scalatest),
  publishTestJar
)
lazy val `tolerance-x` = tolerance.x

lazy val types =
  cross
    .settings(
      group("org.hammerlab"),
      v"1.5.0",
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
lazy val `types-x` = types.x

lazy val utils =
  cross
    .settings(
      v"2.4.0",
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
lazy val `utils-x` = utils.x

lazy val all =
  root(
        `cubic-x`,
       `format-x`,
      `quartic-x`,
        `stats-x`,
    `tolerance-x`,
        `types-x`,
        `utils-x`
  )
