package org.hammerlab.math.polynomial.test

import hammerlab.indent.implicits.spaces4
import hammerlab.iterator._
import hammerlab.lines._
import org.hammerlab.math.polynomial.result.Solve
import hammerlab.math.FromDouble
import hammerlab.show._
import org.hammerlab.Suite
import org.hammerlab.io.print.Limit
import org.hammerlab.math.format.SigFigs
import org.hammerlab.math.polynomial
import org.hammerlab.math.polynomial.result.{ Expected, Results, Stats }
import org.hammerlab.math.polynomial.roots.RootShapes
import org.hammerlab.math.polynomial.roots.dsl.IsRootShapes
import org.hammerlab.math.polynomial.{ ImaginaryRootPair, Real, TestCase, result }
import org.hammerlab.math.syntax.Doubleish._
import org.hammerlab.math.syntax.{ Doubleish, E }
import org.hammerlab.test.CanEq
import org.scalatest.exceptions.TestFailedException
import shapeless.HNil
import spire.algebra.{ Field, IsReal, NRoot, Signed, Trig }
import spire.math.{ Complex, abs }

import scala.math.exp
import scala.util.Random._

trait ShowD[D] {
  /**
   * For pretty-printing purposes, display [[D]]s as corresponding [[Double]] values
   */
  implicit def showD(implicit db: Doubleish[D], sd: Show[Double]): Show[D] =
    Show { _.toDouble.show }
}

abstract class PolySolverTest[T : FromDouble : IsReal : NRoot : Trig](degree: Int)(
    implicit
    // name and `val` these bc mix-ins need them
    val field: Field[T],
    val ordering: Ordering[T],
    val signed: Signed[T],
    val doubleish: Doubleish[T]
)
  extends Suite
    with ShowD[T]
    with IntegerRootSweep[T]
    with IsRootShapes.HasOps
    with RandomTest[T] {

  /** Pretty-printing level */
  implicit var sigfigs: SigFigs = 3

  type D = T

  val N = degree

  /**
   * Subclasses implement solving for roots of a [[TestCase]] here
   */
  implicit def solve(t: TestCase[D]): Seq[Complex[D]]
  implicit def solver: Solve[D] = Solve(solve _)

  implicit val limit: Limit = 3

  type Real    = polynomial.Real[D]
  type Result = result.Result[D]
  type Results = result.Results[D]
  type ResultGroup = result.ResultGroup[D]
  type Expected = result.Expected[D]

  implicit val resultsCanEqExpected: CanEq[Results, Expected] =
    new CanEq[Results, Expected] {
      import org.hammerlab.test.CanEq.Cmp
      val cmpStats = shapeless.the[Cmp[Stats]]
      override type Error = cmpStats.Error
      override def cmp(l: Results, r: Expected): Option[Error] = {
        cmpStats(l.errors, r.errors)
      }
    }

  /** Helpers for converting to [[D]] */
  val fromD = FromDouble[D] _
  implicit val fromInt   : Int    ⇒ D = (x: Int) ⇒ fromD(x)
  implicit val fromDouble: Double ⇒ D = fromD

  implicit def showDouble: Show[Double] = org.hammerlab.math.format.SigFigs.showSigFigs

  def check(name: String,
            shapes: RootShapes,
            cases: Seq[TestCase[D]])(
      implicit
      expecteds: Seq[(RootShapes, Stats)]
  ): Unit = {
    val expected =
      expecteds
        .toMap
        .get(shapes)

    val expectedStr = expected.map(_.show).getOrElse[String]("!!!")
    test(show"$name: shapes ${s"%-${2*N-1}s".format(shapes.show)} expected $expectedStr") {
      val results: Results = cases.iterator
      val actual: Expected = results

      def print(): Unit = {
        import hammerlab.lines.generic._
        val lines = actual.errors.lines
        println(
          show"($shapes) →\n" +
          indent(lines).show + ","
        )
      }

      def err(e: Exception): Unit = {
        import hammerlab.lines.generic._
        val lines = actual.errors.lines
        val msg = lines.show
        print()
//        println(indent(lines).show)
        throw new Exception(msg, e)
      }

      try {
        expected match {
          case Some(expected) ⇒
            implicit val ε: E = 1e-2
            ===(
              actual.errors,
              expected
            )
          case None ⇒
            print()
            throw new Exception(
              show"Missing expected stats for shapes: $shapes"
            )
        }
      } catch {
        case e: TestFailedException ⇒ err(e)
      }
    }
  }

  def gaussian(): D = nextGaussian()

  def logNormal(): D =
    exp(
      nextGaussian()
    ) *
    (
      if (nextBoolean())
         1
      else
        -1
    )

  val casePrintInterval = 1000

  /**
   * Helper for printing status messages while potentially brute-forcing many polynomial-solving iterations, e.g. via
   * [[randomCases]] or [[rootSweep]]
   */
  def printEveryN(elems: Seq[TestCase[T]]): Seq[TestCase[T]] = printEveryN(elems.iterator).toVector
  def printEveryN(it: Iterator[TestCase[T]]): Iterator[TestCase[T]] =
    it
      .zipWithIndex
      .map {
        case (d, idx) ⇒
          if (idx % casePrintInterval == 0)
            println(show"iteration $idx:\t$d")
          d
      }

  implicit def convTuple[L, R](t: (L, R))(implicit f: IsRootShapes[L]): (RootShapes, R) = (f(t._1), t._2)

  implicit def makeResults(cases: Iterator[TestCase[D]]): Results = Results(cases)
  implicit def makeResults(cases: Seq[TestCase[D]]): Results = Results(cases.iterator)
}
