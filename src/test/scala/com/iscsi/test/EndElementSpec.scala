package com.iscsi.test

import org.specs2.mutable._
import scala.util.Random

class EndElementSpec extends Specification {

  import com.iscsi.EndElement._

  val nthElement = 5
  val r = Random

  "Function returning nth element from end of list" should {
    "return None if empty list" in {
      val emptyList = List.empty[Int]
      nthFromEnd(nthElement, emptyList) must be equalTo None
    }

    "return None if list smaller than element index requested" in {
      val notBigEnough = List[Int](1, 2, 3, 4)
      nthFromEnd(nthElement, notBigEnough) must be equalTo None
    }

    "return None if list has same number of elements as index requested" in {
      val sameSize = List[Int](1, 2, 3, 4, 5)
      nthFromEnd(nthElement, sameSize) must be equalTo None
    }

    "return None from list when list exact sized " in {
      val goodList = List[Int](
        //  5  4  3  2  1
        6, 5, 4, 3, 2, 1)
      nthFromEnd(nthElement, goodList) must be equalTo None
    }

    "return nth element from list when list longer than exact sized" in {
      //                     5  4  3  2  1
      val goodList = List[Int](14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
      nthFromEnd(nthElement, goodList) must be equalTo Some(6)
    }

    "return nth element from list when list longer than exact sized" in {
      val mthElement = 8
      //                                                8  7  6  5  4  3  2  1
      val goodList = List[Int](14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
      nthFromEnd(mthElement, goodList) must be equalTo Some(9)
    }

    "return nth element from random list when list longer than exact sized" in {
      val randomList = List.fill(100)(r.nextInt(100))
      nthFromEnd(nthElement, randomList) must be equalTo Some(randomList.takeRight(nthElement + 1).head)
    }
  }

}
