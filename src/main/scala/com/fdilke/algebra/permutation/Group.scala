package com.fdilke.algebra.permutation

import com.fdilke.backtrack.BacktrackIterable
import com.fdilke.backtrack.node.Node
import com.fdilke.backtrack.node.NodeSolvers.StackSafeDedupNodeSolver

import scala.annotation.{tailrec, targetName}
import scala.annotation.targetName
import com.fdilke.backtrack.node.MonadIterable
import com.fdilke.utility.SetsUtilities

import scala.collection.mutable
import scala.compiletime.ops.any.==
import scala.language.postfixOps

trait Group[T]: 
  group =>
  val unit: T
  val elements: Set[T]
  def multiply(element1: T, element2 : T): T
  def invert(element: T): T

  def power(element: T, exponent: Int): T =
    exponent match
      case 0 => unit
      case 1 => element
      case _ =>
        val xn_2 = power(element, exponent/2)
        val xn = multiply(xn_2, xn_2)
        if (exponent % 2 == 1)
          multiply(xn, element)
        else
          xn

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

    lazy val findComplement: Option[group.Subgroup] =
      val targetOrder: Int =
        group.order / subgroup.order
      val outsideConjugates: Set[T] =
        group.elements.diff:
          for
            g <- group.elements
            x <- subgroup.elements
          yield
            group.conjugate(x, g)
      case class CandidateComplement(
        candidate: group.Subgroup
      ) extends Node[CandidateComplement, Iterable, group.Subgroup]:
        override def explore: Iterable[Either[CandidateComplement, group.Subgroup]] =
          if candidate.order == targetOrder then
            Iterable(solution(candidate))
          else
            outsideConjugates.diff:
              candidate.elements
            .map: p =>
              group.generateSubgroup(candidate.elements + p)
            .filter: nextCandidate =>
              nextCandidate.elements.subsetOf(outsideConjugates + group.unit)
            .map: nextCandidate =>
              node(CandidateComplement(nextCandidate))
      StackSafeDedupNodeSolver.allSolutions[CandidateComplement, Iterable, group.Subgroup]:
        CandidateComplement:
          group.trivialSubgroup
      .headOption

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
          group.commute(x, y)

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
        commute(x, y)

  def isAmbivalent: Boolean =
    conjugacyClasses.forall:
      cc =>
      val x: T = cc.head
      cc.contains:
        invert:
          x

  def isStronglyAmbivalent: Boolean =
    conjugacyClasses.forall:
      cc =>
      val x: T = cc.head
      val o = orderOf(x)
      (2 until o).filter: r=>
        SetsUtilities.gcd(r, o) == 1
      .forall: r =>
        cc.contains:
          power(x, r)

  private def commute(x: T, y: T): Boolean =
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

  def subgroups: Set[Subgroup] =
    case class SubgroupCriteria(
      above: Subgroup,
      elementsToUse: Seq[T],
      elementsToExclude: Seq[T]
    ):
      def explore: Either[Seq[SubgroupCriteria], Set[Subgroup]] =
        elementsToUse match
          case Nil =>
            Right:
              Set:
                above
          case x +: rest =>
            if above.elements.contains(x) ||
              elementsToExclude.contains(x)
            then
              Left:
                Seq:
                  SubgroupCriteria(above, rest, elementsToExclude)
            else
              val aboveWithX =
                group.generateSubgroup(above.elements + x)
              Left:
                Seq(
                  SubgroupCriteria(aboveWithX, rest, elementsToExclude),
                  SubgroupCriteria(above, rest, x +: elementsToExclude)
                )
    BacktrackIterable[Seq[SubgroupCriteria], Set[Subgroup]](
      Seq:
        SubgroupCriteria(
          trivialSubgroup,
          elements.toSeq,
          Seq.empty
        )
    ):
      _.map:
        _.explore
    .reduce:
      _ union _

  lazy val conjugacyClasses: Set[Set[T]] =
    val buf: mutable.ListBuffer[Set[T]] =
      mutable.ListBuffer[Set[T]]()
    for
      e <- elements
      if !buf.exists: conjClass =>
        conjClass.contains(e)
    do
      buf.append:
        elements.map: 
          conjugate(e, _)
    buf.toSet
    
  lazy val simple: Boolean =
    conjugacyClasses.forall: conjs =>
      if (conjs.contains(unit))
        true
      else
        generateSubgroup(conjs).order == Group.this.order

object Group:
  extension(group: Group[Permutation])
    def stabilizer(
      points: Int*
    ): group.Subgroup =
      group.Subgroup:
        group.elements.filter: p =>
          points.forall: x =>
            p(x) == x
