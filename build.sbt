
default(
  group("org.hammerlab.math")
)

lazy val cubic = crossProject.settings(
  dep(
    shapeless,
    spire
  )
).dependsOn(
  syntax
)
lazy val cubicJS  = cubic.js
lazy val cubicJVM = cubic.jvm.settings(scalajs.stubs)

lazy val format    = crossProject.settings()
lazy val formatJS  = format.js
lazy val formatJVM = format.jvm.settings(scalajs.stubs)

lazy val math = project.settings(
  group("org.hammerlab"),
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

lazy val quartic = crossProject.settings(
  dep(shapeless)
).dependsOn(
  cubic,
  syntax
)
lazy val quarticJS  = quartic.js
lazy val quarticJVM = quartic.jvm

lazy val stats = project.settings(
  r"1.2.0",
  dep(
    cats,
     io_utils % "4.1.0-SNAPSHOT",
    iterators % "2.0.0",
        spire
  )
).dependsOn(
  math,
  typesJVM
)

lazy val syntax = crossProject.settings(
  v"1.0.0",
  testDeps := Nil
)
lazy val syntaxJS  = syntax.js
lazy val syntaxJVM = syntax.jvm

lazy val types = crossProject.settings(
  group("org.hammerlab"),
  v"1.0.2",
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
  math,
  stats,
    cubicJS,   cubicJVM,
   formatJS,  formatJVM,
  quarticJS, quarticJVM,
   syntaxJS,  syntaxJVM,
    typesJS,   typesJVM
)
