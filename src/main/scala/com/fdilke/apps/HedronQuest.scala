package com.fdilke.apps

import com.fdilke.algebra.permutation.{Group, GroupSugar, Permutation}
import GroupSugar._

// Attempt to find permutations f, v, e of order 4, 5, 2 respectively with fv = e
// These then form the generators of a finite homomorphic image of the von Dyck group D(4, 5, 2)

object HedronQuest extends App:
  private val searchDegree = 10
  val group = Permutation.group(searchDegree)
  given Group[Permutation] = group
  private def elementsOfOrder(n: Int) =
    group.elements.filter:
      _.order == n
  private def conjugacyRepresentatives(elements: Set[Permutation]): Set[Permutation] =
    elements.map:
      _.canonicalConjugate
  val order4s = elementsOfOrder(4)
  val order4sReduced = conjugacyRepresentatives(order4s)
  val order5s = elementsOfOrder(5)
  val order5sReduced = conjugacyRepresentatives(order5s)
  println(s"${order4s.size} -> ${order4sReduced.size} elements of order 4")
  println(s"${order5s.size} -> ${order5sReduced.size} elements of order 5")
  val (loop4: Set[Permutation], loop5: Set[Permutation]) =
    if (order4sReduced.size * order5s.size) < (order4s.size * order5sReduced.size) then
      println("Winnowing 4")
      (order4sReduced, order5s)
    else
      println("Winnowing 5")
      (order4s, order5sReduced)

  case class Hedron(
    f: Permutation,
    v: Permutation,
    subgroup: group.Subgroup
  ):
    val order: Int =
      subgroup.order
  private val hedrons: Set[Hedron] =
    for
      f <- loop4
      v <- loop5
      e = f * v if e.order == 2
    yield
      val subgroup = group.generateSubgroup(f, v)
      Hedron(f, v, subgroup)
  private val orders: Seq[Int] =
    hedrons.map { _.order }.toSeq.sorted
  println("Sorted orders: " + orders.mkString(","))
    
  