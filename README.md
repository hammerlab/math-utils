# math-utils

[![Build Status](https://travis-ci.org/hammerlab/math-utils.svg?branch=master)](https://travis-ci.org/hammerlab/math-utils)
[![codecov](https://codecov.io/gh/hammerlab/math-utils/branch/master/graph/badge.svg)](https://codecov.io/gh/hammerlab/math-utils)

[Math]($math), [stats](#stats), and [miscellaneous numeric](#tolerance) and [type-related](#types) utilities:

- [`format`](#format): format numbers to a given number of significant figures
- [`stats`](#stats): collect+display statistics about collections of numeric elements
- [`tolerance`](#tolerance): "fuzzy-equality" for `Double`s
- [`types`](#types): misc type- and type-class-utilities (auto-derived `Ordering`s and `Monoid`s, a scala-js runtime-predicate, etc.)
- [`utils`](#utils): misc numeric utilities (binomial coefficients, interpolation, etc.)
- [`cubic`](#cubic)/[`quartic`](#quartic): solve cubic/quartic equations in terms of Spire type-classes

## [`format`](format)
[![Maven Central: org.hammerlab.math:::format:1.1.0](https://img.shields.io/badge/org.hammerlab.math:::format-1.1.0-green.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.hammerlab.math%22%20format)

Format numbers to a given number of significant figures:

```scala
import hammerlab.math.sigfigs._
implicit val sf: SigFigs = 3

import cats.syntax.show._

1.2345.show
// 1.23

123.45.show
// 123

123456.0.show
// 123456; same length as "1.23e5", so don't bother abbreviating to scientific notation

1234567.0.show
// 1.23e6

0.0000123456.show
// 1.23e-5
```

## [`stats`](stats)

[![Maven Central: org.hammerlab.math:::stats:1.3.2](https://img.shields.io/badge/org.hammerlab.math:::stats-1.3.2-green.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.hammerlab.math%22%20stats)

[`org.hammerlab.stats.Stats`](stats/shared/src/main/scala/org/hammerlab/stats/Stats.scala) has APIs for ingesting numeric elements and outputting nicely formatted statistics about them; modeled after [Apache commons-math `DescriptiveStatistics`](https://github.com/apache/commons-math/blob/MATH_3_6_1/src/main/java/org/apache/commons/math3/stat/descriptive/DescriptiveStatistics.java):

```scala
// Context for displaying ints, longs, and doubles
import cats.syntax.show._
import cats.instances.int.catsStdShowForInt
import cats.instances.long.catsStdShowForLong
import hammerlab.math.sigfigs._
implicit val sf: SigFigs = 2

// Stats are displayed on multiple lines
import hammerlab.lines._
import hammerlab.indent.spaces

// Simulate some doubles to collect statistics about
import util.Random._; setSeed(123)
val samples = Array.fill(100)(nextGaussian)

import hammerlab.stats._

// Create and display a Stats:
val stats = Stats(samples)
stats.showLines
// N: 100, μ/σ: -0.02/1, med/mad: -0.038/0.71
//  elems: -1.4 0.63 0.23 0.28 0.18 -0.37 1.4 0.36 -0.21 1 … -0.4 -1.6 -0.11 -1.1 -0.39 1.5 -0.17 1.6 0.25 -0.25
// sorted: -3.1 -2.2 -1.9 -1.9 -1.7 -1.6 -1.4 -1.4 -1.4 -1.4 … 1.3 1.4 1.4 1.5 1.5 1.5 1.6 1.7 1.8 2.8
//   .01:	-3.1
//   .05:	-1.7
//   .10:	-1.4
//   .25:	-0.75
//   .50:	-0.038
//   .75:	0.67
//   .90:	1.3
//   .95:	1.5
//   .99:	2.7
```

As a bonus, [it can ingest numbers in histogram-style / run-length-encoded format](stats/shared/src/main/scala/org/hammerlab/stats/Stats.scala#L39), supporting `Long` values as well for computations involving element counts from RDDs: 

```scala
// Create a Stats from a histogram:   
Stats.fromHist(
  List(
    1 → 10000000000L,
    2 →  1000000000L,
    1 →         100L,
    2 →  1000000000L
  )
)
.showLines
// N: 12000000100, μ/σ: 1.2/0.37, med/mad: 1/0
//  elems: 1×10000000000 2×1000000000 1×100 2×1000000000
// sorted: 1×10000000100 2×2000000000
//   .75:	1
//   .90:	2
```

## [`tolerance`](tolerance)

[![Maven Central: org.hammerlab.math:::tolerance:1.0.0](https://img.shields.io/badge/org.hammerlab.math:::tolerance-1.0.0-green.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.hammerlab.math%22%20tolerance)

Fuzzy-equality for `Double`s:

```scala
import hammerlab.math.tolerance._

// let things be "equal" that are within 1% of one another
implicit val ε: E = 1e-2
2.0  === 2.02  //  true
2.02 === 2.04  //  true
2.0  === 2.03  // false!
```

[hammerlab/test-utils](https://github.com/hammerlab/test-utils/) `Suite` interfaces [come with a (configurable) `1e-6` fuzz-factor for `Double` comparisons](https://github.com/hammerlab/test-utils/blob/v1.0.1/suite/shared/src/main/scala/org/hammerlab/Suite.scala#L34-L35)

## [`types`](types)

[![Maven Central: org.hammerlab:::types:1.3.1](https://img.shields.io/badge/org.hammerlab:::types-1.3.1-green.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.hammerlab%22%20types)

### `either`/`or`
Shorthands for `Either` and "or" ([`cats.Ior`](https://typelevel.org/cats/datatypes/ior.html)) subclasses:

```scala
import hammerlab.either._
import hammerlab.or._
```

### `collection`

`CanBuildFrom` instances for `Array`s and `Vector`s:

  ```scala
  import hammerlab.collection._
  ```

### `monoid`
  
`cats.Monoid` derivations for products and coproducts:

```scala
import hammerlab.monoid._
case class Samples(num: Int, sum: Long)

Samples(10, 1000) |+| Samples(20, 2000)
// Samples(30, 3000)
```

### `option`

Some `Option`-related helpers:

#### `Opt`
Wrapper class for `Option` that implicitly wraps values in a `Some` where necessary:

```scala
import hammerlab.option._

// remove default-argument boilerplate
def foo(n: Opt[Int] = 111) = { ??? }

// remove call-site boilerplate
foo(333)

// degrade gracefully to regular Option
foo(Some(222))
foo(None)
```

#### `?` operator
Create a `Some` iff a predicate is true:

```scala
(20 > 10) ? "abc"
// Some(abc)

(20 < 10) ? "abc"
// None
```

### `scalajs`
Predicates for specializing behavior on the JVM vs JS:

```scala
import hammerlab.scalajs._
js (111)(222)  // 111 in JS, 222 in JVM
JS (111)(222)  // same
222 js_? 111   // same 

jvm(111)(222)  // 111 in JVM, 222 in JS
JVM(111)(222)  // same
222 jvm_? 111  // same
``` 

### `str`

`Name` (alias `Str`) type that is implicitly-constructible from a `String` or `Symbol`:

```scala
import hammerlab.str._
val a: Str = "abc"
val b: Str = 'def
```

## [`utils`](math)

[![Maven Central: org.hammerlab.math:::utils:2.2.1](https://img.shields.io/badge/org.hammerlab.math:::utils-2.2.1-green.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.hammerlab.math%22%20utils)

Miscellaneous numeric utilities

```scala
import hammerlab.math._
```

### `min`
Overloads of `math.min` that take an `Int` and a `Long` and return an `Int

### HyperGeometric Distribution
[`hammerlab.math.utils.HypergeometricDistribution`](utils/shared/src/main/scala/org/hammerlab/math/HypergeometricDistribution.scala) is an implementation of a hypergeometric distribution, modeled after [`org.apache.commons.math3.distribution.HypergeometricDistribution`](https://commons.apache.org/proper/commons-math/javadocs/api-3.6/org/apache/commons/math3/distribution/HypergeometricDistribution.html), but supporting 8-byte `Long` parameters.

### `div`

Up-rounding integer-division:

```scala
div(20 , 10)  // 2
div(21L, 10)  // 3
``` 

### `Steps`

```scala
geometricSteps(100, 15)
// SortedSet(0, 1, 2, 3, 4, 5, 6, 8, 11, 17, 24, 34, 49, 70, 100)

roundNumbers(100)
// SortedSet(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
```

## [`cubic`](cubic), [`quartic`](quartic)

[![Maven Central: org.hammerlab.math:::cubic:1.0.0](https://img.shields.io/badge/org.hammerlab.math:::cubic-1.0.0-green.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.hammerlab.math%22%20cubic) &nbsp; [![Maven Central: org.hammerlab.math:::quartic:1.0.0](https://img.shields.io/badge/org.hammerlab.math:::quartic-1.0.0-green.svg)](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22org.hammerlab.math%22%20quartic)

Solve cubics with `Double` coefficients (and `Complex[Double]` roots):

```scala
import cubic.complex._
import spire.implicits._

val dbl = Cubic.doubleComplex[Double]

// Solutions to x³ + 2x² + 3x + 4 = 0:
dbl(1, 2, 3, 4)
// List(
//   -0.175 + 1.547i,
//   -1.651 - 4.457e-16i,
//   -0.175 - 1.54i
// )

// x³ + 3x² + 3x + 1 = 0:
dbl.monic(3, 3, 1)
List(
  -1.0 - 0.0i,
  -1.0 + 0.0i,
  -1.0 + 0.0i
)

// x³ + 4x + 6 = 0:
dbl.depressed(4, 6)
// List(
//   0.567 + 2.228i,
//   -1.135 + -6.594e-16i,
//   0.567 + -2.228i
// )
```

// Solve cubics with `BigDecimal` coefficients (and `Complex[BigDecimal]` roots):

```scala
val bigdbl = Cubic.doubleComplex[BigDecimal]

bigdbl(1, 2, 3, 4)
List(
  -0.17468540428030620349438212279655060000000000000000000000000000000000000 + 1.546868887231396634770605370742269000000000000000000000000000000000000000i, 
  -1.650629191439388248503394865883312800000000000000000000000000000000000000000000000000 + -3.43495611274383380622701277675890600000000000000000000000E-16i,
  -0.17468540428030587574830256705833460000000000000000000000000000000000000 + -1.546868887231396291274994096359002000000000000000000000000000000000000000i
)
```

Quartics work similarly:
```scala
import quartic.complex._

// Solve quartics with Double coefficients
val dbl = Quartic.doubleComplex[Double]

// Solve quartics with BigDouble coefficients
val bigdbl = Quartic.doubleComplex[BigDecimal]

dbl(a, b, c, d, e)
dbl.monic(b, c, d, e)
dbl.depressed(p, q, r)
```
