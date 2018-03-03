
default(
  group("org.hammerlab.math"),
  versions(
     io_utils → "5.0.0".snapshot,
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
  format % "test->test",
  math,
  syntax,
  types % "test"
)
lazy val cubicJS  = cubic.js
lazy val cubicJVM = cubic.jvm

lazy val format = crossProject.settings(
  r"1.0.0",
  dep(cats),
  testDeps := Seq(scalatest)
)
lazy val formatJS  = format.js
lazy val formatJVM = format.jvm

lazy val math = crossProject.settings(
  group("org.hammerlab"),
  r"2.2.0",
  dep(
    cats,
    shapeless,
    spire
  ),
  consolePkg("hammerlab.math")
).dependsOn(
  format
)
lazy val mathJS  = math.js
lazy val mathJVM = math.jvm.settings(
  // a couple components depend on Java-only libraries and are only included in the JVM module
  dep(
    commons_math,
    kryo tests
  )
)

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
  cubic % "compile->compile;test->test",
  format,
  math,
  syntax,
  types % "test"
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
    math,
  syntax,
   types
)
lazy val statsJS  = stats.js
lazy val statsJVM = stats.jvm

lazy val syntax = crossProject.settings(
  r"1.0.0",
  dep(
    cats,
    spire
  ),
  // test-utils depends on this module for fuzzy-equality / tolerant-double comparisons, and dependency-resolvers
  // emit circular-dependency false-positives when `a` depends on `b` and `b` depends on `a`'s tests
  testDeps := Seq(scalatest)
).dependsOn(
  format
)
lazy val syntaxJS  = syntax.js
lazy val syntaxJVM = syntax.jvm

lazy val types = crossProject.settings(
  group("org.hammerlab"),
  r"1.1.0",
  dep(
    cats,
    shapeless,
    spire
  )
)
lazy val typesJS  = types.js
lazy val typesJVM = types.jvm

lazy val math_utils = rootProject(
  "math-utils",
    cubicJS,   cubicJVM,
   formatJS,  formatJVM,
     mathJS,    mathJVM,
  quarticJS, quarticJVM,
    statsJS,   statsJVM,
   syntaxJS,  syntaxJVM,
    typesJS,   typesJVM
)
