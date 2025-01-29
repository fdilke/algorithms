package com.fdilke.algebra.permutation

import scala.language.postfixOps

case class Permutation(
  images: Int*
):
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

object Permutation:
  def identity(degree: Int): Permutation =
    Permutation(
      0 until degree *
    )
    
  def enumerate(degree: Int): Set[Permutation] =
    ((0 until degree) permutations).map:
      p => Permutation(p.toSeq *)
    .toSet

  def group(degree: Int): Group[Permutation] =
    new Group[Permutation]:
      override val unit: Permutation =
        Permutation.identity(degree)

      override val elements: Set[Permutation] =
        enumerate(degree)

      override def multiply(
        p1: Permutation,
        p2: Permutation
      ): Permutation =
        p1(p2)

      override def invert(element: Permutation): Permutation =
        element.inverse

