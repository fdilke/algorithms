package com.fdilke.algebra.permutation

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
    
  def symmetricGroup(degree: Int): Group[Permutation] =
    group:
      enumerate:
        degree

  def alternatingGroup(degree: Int): Group[Permutation] =
    group:
      Permutation.symmetricGroup(degree).elements.filter:
        _.parity == 1
