package com.fdilke.algebra.permutation

import scala.annotation.targetName
import scala.math.Ordering.Implicits.infixOrderingOps

object GroupSugar:
  implicit class MultiplicationWrapper[
    T
  ](
     element: T
  )(
   implicit group: Group[T]
  ):
    @targetName("star")
    def *(element2: T): T =
      group.multiply(element, element2)

    @targetName("exponentConjugate")
    def ^(element2: T): T =
      group.conjugate(element, element2)

    @targetName("unaryInverse")
    def unary_~ : T =
      group.invert(element)

    def order: Int =
      group.orderOf(element)

    def canonicalConjugate(
      using Ordering[T]
    ): T =
      group.elements.toSeq.map:
        group.conjugate(element, _)
      .min
