
default(
  group("org.hammerlab.math"),
  versions(
     io_utils → "5.1.1",
    iterators → "2.1.0"
  )
)

lazy val cubic = crossProject.settings(
  v"1.0.0",
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
lazy val cubicJS  = cubic.js
lazy val cubicJVM = cubic.jvm
lazy val cubicX   = parent(cubicJS, cubicJVM)

lazy val format = crossProject.settings(
  v"1.0.0",
  dep(cats)
)
lazy val formatJS  = format.js
lazy val formatJVM = format.jvm
lazy val formatX   = parent(formatJS, formatJVM)

lazy val quartic = crossProject.settings(
  v"1.0.0",
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
lazy val quarticJS  = quartic.js
lazy val quarticJVM = quartic.jvm
lazy val quarticX   = parent(quarticJS, quarticJVM)

lazy val stats = crossProject.settings(
  v"1.3.2",
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
lazy val statsJS  = stats.js
lazy val statsJVM = stats.jvm
lazy val statsX   = parent(statsJS, statsJVM)

lazy val tolerance = crossProject.settings(
  v"1.0.0",
  dep(cats),
  // test-utils depends on this module for fuzzy-equality / tolerant-double comparisons, and dependency-resolvers
  // emit circular-dependency false-positives when `a` depends on `b` and `b` depends on `a`'s tests
  testDeps := Seq(scalatest),
  publishTestJar
)
lazy val toleranceJS  = tolerance.js
lazy val toleranceJVM = tolerance.jvm
lazy val toleranceX   = parent(toleranceJS, toleranceJVM)

lazy val types = crossProject.settings(
  group("org.hammerlab"),
  v"1.3.1",
  dep(
    cats,
    shapeless,
    spire
  )
)
lazy val typesJS  = types.js
lazy val typesJVM = types.jvm.settings(
  http4s.version := "0.18.13",
  dep(
    // UrlTest runs a dummy server
    http4s.dsl tests,
    http4s.`blaze-server` tests
  )
)
lazy val typesX   = parent(typesJS, typesJVM)

lazy val utils = crossProject.settings(
  v"2.2.1",
  dep(
    cats,
    shapeless,
    spire
  ),
  consolePkg("hammerlab.math")
)
lazy val utilsJS  = utils.js
lazy val utilsJVM = utils.jvm.settings(
  // a couple components depend on Java-only libraries and are only included in the JVM module
  dep(
    commons.math,
    kryo tests
  )
)
lazy val utilsX = parent(utilsJS, utilsJVM)

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
