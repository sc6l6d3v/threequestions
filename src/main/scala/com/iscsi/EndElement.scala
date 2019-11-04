package com.iscsi

object EndElement {

  /**
   * As per Scala Lang migration notes for 2.13:
   * https://docs.scala-lang.org/overviews/core/collections-migration-213.html
   */

  /**
   * Find nth element from end of List
   *    performing empty check designating -1 as failure
   * @param n element #
   * @param ll number list
   * @return option of that element
   */
  def nthFromEnd(n: Int, ll: List[Int]): Option[Int] = if (ll.isEmpty) None else walkList(n, ll)

  /**
   * Using two pointers to positions nth elements apart on the original list, walk to the end of the
   *    shorter list in tandem with the longer list finding the element nth away from the end.
   *    endList references shorter list, while frontList references longer list.
   *    Limits of loop in for comprehension are determined by original list.
   *    Monotonically incrementing functional pointer from Stream iterator.
   *    Note nth element from end requires at least n + 1 elements in list.
   *    First gate checks if anything remaining on endList, if so we can notch one to the right and shorten
   *      that list as well as the frontList.
   *    Second gate checks if we have ran out of items in the endList, we must be n elements away on the head
   *      of the frontList.
   *      We iterate a few more times consuming the remaining elements of the inputList up to the first gate
   *      returning a list of options of Int, the nth element from the end.
   *    Wartremover adds a layer of protection to the head that would be typically used.
   * @param nth element to find
   * @param inputList bunch of numbers
   * @return
   */
  private def walkList(nth: Int, inputList: List[Int]): Option[Int] = {
    val strIter = Stream.from(0).iterator
    val backPtr = inputList.drop(nth + 1)
    if (backPtr.isEmpty) None
    else {
      val opts =
        for {
          _ <- inputList
          i = strIter.next
          backList = backPtr.drop(i)
          if backList.nonEmpty
          endList = backList.drop(1)
          frontList = inputList.drop(i).drop(1)
          if endList.isEmpty
          res = frontList.headOption
        } yield res
      opts.flatten.headOption
    }
  }
}
