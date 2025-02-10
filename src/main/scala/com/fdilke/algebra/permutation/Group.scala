package com.fdilke.algebra.permutation

import com.fdilke.backtrack.node.Node
import com.fdilke.backtrack.node.NodeSolvers.StackSafeDedupNodeSolver

import scala.annotation.{tailrec, targetName}
import scala.annotation.targetName
import com.fdilke.backtrack.node.MonadIterable

trait Group[T]: 
  group =>
  val unit: T
  val elements: Set[T]
  def multiply(element1: T, element2 : T): T
  def invert(element: T): T

  lazy val order: Int =
    elements.size

  def conjugate(x: T, y : T): T =
    multiply(
      invert(y),
      multiply(x, y)
    )

  case class Subgroup(
     override val elements: Set[T]
  ) extends Group[T]:
    subgroup =>
    def isNormal: Boolean =
      group.elements.forall: x =>
        subgroup.elements.subsetOf:
          subgroup.elements.map: y =>
            group.conjugate(y, x)

    lazy val hasComplement: Option[group.Subgroup] =
      val targetOrder: Int =
        group.order / subgroup.order
      val outsideConjugates: Set[T] =
        group.elements.diff:
          for
            g <- group.elements
            x <- subgroup.elements
          yield
            group.conjugate(x, g)
//      println("outside conjs: " + outsideConjugates.size)
      case class CandidateComplement(
        candidate: group.Subgroup
      ) extends Node[CandidateComplement, Iterable, group.Subgroup]:
        override def explore: Iterable[Either[CandidateComplement, group.Subgroup]] =
//          println("candidate has order: " + candidate.order)
          if candidate.order == targetOrder then
            Iterable(solution(candidate))
          else
            val pool: Set[T] =
              outsideConjugates.diff(candidate.elements)
//            println("pool size: " + pool.size)
//            println("pool: " + pool)
            val nextCandidates: Iterable[group.Subgroup] =
              pool.map: p =>
                group.generateSubgroup(candidate.elements + p)
              .filter: nextCandidate =>
//                println("next candidate, order:" + nextCandidate.order)
                nextCandidate.elements.subsetOf(outsideConjugates + group.unit)
            nextCandidates.map: nextCandidate =>
              node(CandidateComplement(nextCandidate))
      StackSafeDedupNodeSolver.allSolutions[CandidateComplement, Iterable, group.Subgroup](
        CandidateComplement(group.trivialSubgroup)
      ).headOption

    override val unit: T = group.unit

    override def multiply(element1: T, element2: T): T =
      group.multiply(element1, element2)

    override def invert(element: T): T =
      group.invert(element)

    def contains(other: group.Subgroup): Boolean =
      other.elements.subsetOf(elements)

    @targetName("exponentConjugate")
    def ^(x: T): group.Subgroup =
      group.Subgroup:
        elements.map:
          conjugate(_, x)

  lazy val trivialSubgroup: Subgroup =
    Subgroup(Set(unit))

  lazy val wholeGroup: Subgroup =
    Subgroup(elements)

  lazy val centre: Subgroup =
    Subgroup:
      elements filter: x =>
        elements forall: y =>
          group.commutes(x, y)

  def generateSubgroup(generators: T*): Subgroup =
    generateSubgroup(generators.toSet)

  private def multiplySets(
    set1: Set[T],
    set2: Set[T]
  ): Set[T] =
    for (x <- set1; y <- set2)
      yield group.multiply(x, y)

  def generateSubgroup(generators: Set[T]): Subgroup =
    @tailrec def generate(
      t: Set[T],
      x: Set[T]
    ): Set[T] =
      val xg = multiplySets(x, generators)
      val xg_t = xg -- t
      if (xg_t.isEmpty)
        t
      else
        generate(t ++ xg_t, xg_t)
    Subgroup:
      generate(generators, generators) + group.unit

  def orderOf(x: T): Int =
    generateSubgroup(x).order

  def isCyclic: Boolean =
    elements.exists: candidateGenerator =>
      generateSubgroup(candidateGenerator) == wholeGroup

  def isAbelian: Boolean =
    elements.forall: x =>
      elements.forall: y =>
        commutes(x, y)

  def commutes(x: T, y: T): Boolean =
    multiply(x, y) == multiply(y, x)

  def elementOrders: Map[Int, Int] =
    group.elements.toSeq.map:
      group.orderOf
    .groupBy(identity)
    .view.mapValues:
      _.size
    .toMap

  def elementOfOrder(
    n: Int
  ): T =
    elements.find: g =>
      orderOf(g) == n
    .getOrElse:
      throw new IllegalArgumentException(s"no element of order $n found")
