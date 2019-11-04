package com.iscsi

import org.scalactic.Equality
//import org.scalactic.TypeCheckedTripleEquals._
import org.scalactic.TripleEquals._
//import org.scalactic.Tolerance._

case class Edge(length: Int) {
  def +(e: Edge): Edge = Edge(length + e.length)

  def >(e: Edge): Boolean = length > e.length

  def <(e: Edge): Boolean = length < e.length

  //  def ==(e: Edge): Boolean = length == e.length

  //  def !=(e: Edge): Boolean = length != e.length

  implicit val edgeEq =
    new Equality[Edge] {
      override def areEqual(a: Edge, b: Any): Boolean =
        b match {
          case e: Edge => e.length === a.length
          case _       => false
        }
    }

  override def toString: String = s"$length "
}

abstract class Triangle(e1: Edge, e2: Edge, e3: Edge) {

  def isValid: Boolean =
    e1 + e2 > e3 &&
      e1 + e3 > e2 &&
      e2 + e3 > e1

  def isType: Boolean

  def edges: String = s"sides ($e1, $e2, $e3)"

  def name: String
}

case class Equilateral(e1: Edge, e2: Edge, e3: Edge) extends Triangle(e1: Edge, e2: Edge, e3: Edge) {
  override def isType: Boolean = e1 === e2 && e2 === e3

  override def name: String = "equilateral"
}

case class Isosceles(e1: Edge, e2: Edge, e3: Edge) extends Triangle(e1: Edge, e2: Edge, e3: Edge) {
  override def isType: Boolean = e1 === e2 || e2 === e3 || e1 === e3

  override def name: String = "isosceles"
}

case class Scalene(e1: Edge, e2: Edge, e3: Edge) extends Triangle(e1: Edge, e2: Edge, e3: Edge) {
  override def isType: Boolean = (e1 !== e2) && (e2 !== e3) && (e1 !== e3)

  override def name: String = "scalene"
}

object TriangleChecker extends App {

}
