package com.fdilke.algebra.permutation

import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.crossCheckResult

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.math.Ordered.orderingToOrdered

case class Permutation(
  images: Int*
) extends Comparable[Permutation]:
  val degree: Int = images.size

  def apply(index: Int): Int =
    images(index)

  def apply(other: Permutation): Permutation =
    if degree == other.degree then
      Permutation(
        images map { other(_) } *
      )
    else throw new IllegalArgumentException("degrees differ, cannot compose")

  def inverse: Permutation =
    val array: Array[Int] = new Array[Int](degree)
    0 until degree foreach: index =>
      array(this(index)) = index
    Permutation(array*)

  def parity: Int =
    val array: Array[Int] =
      images.toArray
    var flag = 1
    for
      index <- array.indices
    do
      if array(index) != index then
        var other = index
        while index != array(other) do
          other = array(other)
        array(other) = array(index)
        array(index) = index
        flag = -flag
    flag

  override def compareTo(that: Permutation): Int =
    if degree != that.degree then
      throw new IllegalArgumentException("cannot compare permutations of different degrees")
    else
      images.compareTo(that.images)

  override def toString: String =
    s"""Permutation(${images.mkString(", ")})"""

object Permutation:
  def identity(degree: Int): Permutation =
    Permutation(
      0 until degree *
    )

  def enumerate(degree: Int): Set[Permutation] =
    ((0 until degree) permutations).map:
      p => Permutation(p.toSeq *)
    .toSet

  def group(
    permutations: Set[Permutation]
  ): Group[Permutation] =
    if permutations.isEmpty then
      throw new IllegalArgumentException("empty permutation group")
    else
      val degree = permutations.head.degree
      new Group[Permutation]:
        override val unit: Permutation =
          Permutation.identity(degree)
  
        override val elements: Set[Permutation] =
          permutations
  
        override def multiply(
          p1: Permutation,
          p2: Permutation
        ): Permutation =
          p1(p2)
  
        override def invert(
          element: Permutation
        ): Permutation =
          element.inverse

  private def multiplySets(
    set1: Set[Permutation],
    set2: Set[Permutation]
  ): Set[Permutation] =
    for
      x <- set1
      y <- set2
    yield
      x(y)

  def group(
    generatorsSeq: Permutation*
  ): Group[Permutation] =
    if generatorsSeq.isEmpty then
      group(Set(Permutation.identity(0)))
    else
      crossCheckResult:
        generatorsSeq.map: p =>
          () => p.degree
      .match
        case None =>
          throw new IllegalArgumentException("degrees do not match")
        case Some(degree) =>
          val generators: Set[Permutation] =
            generatorsSeq.toSet
          @tailrec def generate(
            t: Set[Permutation],
            x: Set[Permutation]
          ): Set[Permutation] =
            val xg = multiplySets(x, generators)
            val xg_t = xg -- t
            if (xg_t.isEmpty)
              t
            else
              generate(t ++ xg_t, xg_t)
          group:
            generate(generators, generators) + Permutation.identity(degree)

  def symmetricGroup(
    degree: Int
  ): Group[Permutation] =
    group:
      enumerate:
        degree

  def alternatingGroup(
    degree: Int
  ): Group[Permutation] =
    group:
      Permutation.symmetricGroup:
        degree
      .elements.filter:
        _.parity == 1
