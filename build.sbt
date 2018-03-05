
default(
  group("org.hammerlab.math"),
  versions(
     io_utils → "5.0.0",
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
  )
).dependsOn(
  tolerance,
      utils,
     format,
      types test
)
lazy val cubicJS  = cubic.js
lazy val cubicJVM = cubic.jvm

lazy val format = crossProject.settings(
  v"1.0.0",
  dep(cats)
)
lazy val formatJS  = format.js
lazy val formatJVM = format.jvm

lazy val quartic = crossProject.settings(
  v"1.0.0",
  dep(
    cats,
    io_utils,
    shapeless,
    spire,
    iterators.tests
  )
).dependsOn(
     format,
  tolerance,
      utils,
      cubic andTest,
      types test
)
lazy val quarticJS  = quartic.js
lazy val quarticJVM = quartic.jvm

lazy val stats = crossProject.settings(
  v"1.3.0",
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

lazy val tolerance = crossProject.settings(
  v"1.0.0",
  dep(cats),
  // test-utils depends on this module for fuzzy-equality / tolerant-double comparisons, and dependency-resolvers
  // emit circular-dependency false-positives when `a` depends on `b` and `b` depends on `a`'s tests
  testDeps := Seq(scalatest)
)
lazy val toleranceJS  = tolerance.js
lazy val toleranceJVM = tolerance.jvm

lazy val types = crossProject.settings(
  group("org.hammerlab"),
  v"1.1.0",
  dep(
    cats,
    shapeless,
    spire
  )
)
lazy val typesJS  = types.js
lazy val typesJVM = types.jvm

lazy val utils = crossProject.settings(
  v"2.2.0",
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
    commons_math,
    kryo tests
  )
)

lazy val math_utils = rootProject(
  "math-utils",
      cubicJS,     cubicJVM,
     formatJS,    formatJVM,
       utilsJS,      utilsJVM,
    quarticJS,   quarticJVM,
      statsJS,     statsJVM,
  toleranceJS, toleranceJVM,
      typesJS,     typesJVM
)
