package com.iscsi

object ListInclusion {

  def includes(l1: List[Int], l2: List[Int]): Boolean =
    l1.forall(elt => l2.contains(elt))

}
