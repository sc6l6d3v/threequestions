package com.iscsi.test

import org.specs2.mutable.Specification
import org.specs2.specification.core.{ Fragment, Fragments }

class TrianglesSpec extends Specification {

  import com.iscsi._

  val (e1, e2, e3, e4, e5) = (Edge(3), Edge(4), Edge(5), Edge(7), Edge(9))

  // build triangles and expected check
  val triangleTests: Map[Triangle, Boolean] = Map(
    Equilateral(e1, e1, e1) -> true,
    Equilateral(e1, e1, e2) -> false,
    Equilateral(e1, e2, e1) -> false,
    Equilateral(e1, e2, e2) -> false,
    Equilateral(e1, e2, e3) -> false,
    Isosceles(e1, e1, e1) -> true,
    Isosceles(e1, e1, e2) -> true,
    Isosceles(e1, e2, e1) -> true,
    Isosceles(e2, e1, e1) -> true,
    Isosceles(e1, e2, e3) -> false,
    Scalene(e1, e1, e1) -> false,
    Scalene(e1, e1, e2) -> false,
    Scalene(e1, e2, e2) -> false,
    Scalene(e1, e2, e1) -> false,
    Scalene(e1, e2, e3) -> true)

  "Determine type of triangle and whether valid" should {
    // Dynamically generate the tests by iterating over the keys of the map
    Fragments.foreach(triangleTests.keys.toSeq)(checkTriangle)
  }

  def checkTriangle(tri: Triangle): Fragment = {
    val bool = triangleTests(tri)
    val check = tri.isType && tri.isValid
    s"return $bool when checking $tri" in {
      if (bool) check must beTrue
      else check must beFalse
    }
  }
}
