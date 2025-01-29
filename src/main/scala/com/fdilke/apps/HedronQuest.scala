package com.fdilke.apps

import com.fdilke.algebra.permutation.{Group, GroupSugar, Permutation}
import GroupSugar._

// Attempt to find permutations f, v, e of order 4, 5, 2 respectively with fv = e
// These then form the generators of a finite homomorphic image of the von Dyck group D(4, 5, 2)

object HedronQuest extends App:
  private val searchDegree = 7
  val group = Permutation.group(searchDegree)
  given Group[Permutation] = group
  private def elementsOfOrder(n: Int) =
    group.elements.filter:
      _.order == n
  val order4s = elementsOfOrder(4)
  val order5s = elementsOfOrder(5)
  case class Hedron(
    subgroup: group.Subgroup
  ):
    val order: Int =
      subgroup.order
  private val hedrons: Set[Hedron] =
    for
      f <- order4s
      v <- order5s
      e = f * v if e.order == 2
    yield
      val subgroup = group.generateSubgroup(f, v)
      Hedron(subgroup)
  private val orders: Seq[Int] =
    hedrons.map { _.order }.toSeq.sorted
  println("Sorted orders: " + orders.mkString(","))
    
  