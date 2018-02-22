package org.hammerlab.math.polynomial.test

import hammerlab.indent.implicits.spaces4
import hammerlab.iterator._
import hammerlab.show._
import org.hammerlab.math.polynomial.result.Stats
import org.hammerlab.math.polynomial.roots.{ RootShape, RootShapes }
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, Real, TestCase }
import spire.math.abs

import scala.Seq.fill
import scala.util.Random.setSeed

trait RandomTest[T] {

  self: PolySolverTest[T] ⇒

  /**
   * Test this many random roots-values for each overall "shape" (number and multiplicity of real and imaginary roots)
   */
  val iterationsPerRootShape: Int

  def gaussians(elems: (RootShapes, Stats)*): Unit = {
    randoms(
      "gaussian",
      gaussian,
      elems
    )
  }

  def logNormals(elems: (RootShapes, Stats)*): Unit = {
    randoms(
      "log-normal",
      logNormal,
      elems
    )
  }

  def randoms(implicit
              name: String,
              rnd: () ⇒ D,
              expecteds: Seq[(RootShapes, Stats)]): Unit =
    for {
      shapes ← RootShapes(N)
      cases = randomCases(name, rnd, shapes)
    } {
      check(
        show"$iterationsPerRootShape $name random cases",
        shapes,
        cases
      )
    }

  /**
   * Generate [[iterationsPerRootShape]] [[TestCase]]s – root-sets of polynomials of degree [[N]], along with derived
   * coefficients – for each possible roots-"shape" ({number of imaginary-root-pairs} X {multiplicity of real and
   * imaginary roots}).
   *
   * The free variables (values of real roots, and real and imaginary parts for imaginary-root-pairs, and a "scale" to
   * multiply all coefficients by) sampled from a standard normal distribution.
   */
  def randomCases(name: String,
                  rnd: () => D,
                  shapes: RootShapes): Seq[TestCase[D]] = {
    /* deterministic seed based on [[name]] and [[shapes]] */
    setSeed(show"$name$shapes".hashCode)
    val RootShapes(realShapes, imagShapes) = shapes
    for {
      _ ← 1 to iterationsPerRootShape

      reals =
        realShapes
          .toSeq
          .flatMap {
            case RootShape(arity, reps) ⇒
              fill(
                reps
              )(
                Real(rnd()) → arity
              )
          }

      imags =
        imagShapes
          .toSeq
          .flatMap {
            case RootShape(arity, reps) ⇒
              fill(
                reps
              )(
                ImaginaryRootPair(rnd(), abs(rnd())) → arity
              )
          }

      scale = rnd()
    } yield
      TestCase(
        reals,
        imags,
        scale
      )
  }
}
