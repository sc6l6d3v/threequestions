package com.iscsi.test

import org.specs2.mutable.Specification

class ListInclusionSpec extends Specification {

  import com.iscsi.ListInclusion._

  val l1 = List(1, 2, 3)
  val l2 = List(1, 2, 3, 4, 5)
  val l3 = List(1, 3, 5)
  val l4 = List(4, 5, 6)

  def listString(l: List[Int]): String = l.mkString(",")

  "Function checking if all elements of one list are in the other" should {
    "return true if empty list is tested against non-empty list" in {
      includes(List.empty[Int], l1) must beTrue
    }

    "return false if non-empty list is tested against empty list" in {
      includes(l1, List.empty[Int]) must beFalse
    }

    "return false if non-empty list is tested against subset non-empty list" in {
      includes(l2, l1) must beFalse
    }

    "return false if non-empty list is tested against disjoint non-empty list" in {
      includes(l3, l4) must beFalse
    }

    "return false if non-empty list is tested against disjoint non-empty list" in {
      includes(l3, l4) must beFalse
    }

    "return false if non-empty list is tested against partial subset non-empty list" in {
      includes(l4, l2) must beFalse
    }

    "return true if non-empty subset list is tested against non-empty list" in {
      includes(l1, l2) must beTrue
      includes(l3, l2) must beTrue
    }
  }
}
