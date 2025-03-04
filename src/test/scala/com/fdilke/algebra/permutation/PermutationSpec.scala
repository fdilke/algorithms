package com.fdilke.algebra.permutation

import munit.FunSuite

import com.fdilke.utility.RichFunSuite._
import scala.math.Ordered.orderingToOrdered

class PermutationSpec extends FunSuite:
  test("Permutations of a given degree can be created and composed"):
    val p1 = Permutation(1,0,2)
    val p2 = Permutation(0,2,1)
    p1.degree is 3
    p1(1) is 0
    p1(p2) is Permutation(2,0,1)

  test("Permutations of different degrees can't be combined"):
    intercept[IllegalArgumentException]:
      val p1 = Permutation(1, 0)
      val p2 = Permutation(0, 2, 1)
      p1(p2)

  test("Permutations equivalent to the identity can be created for any degree"):
    Permutation.identity(4) is Permutation(0, 1, 2, 3)

  test("can be inverted"):
    Permutation(1,0,2).inverse is
      Permutation(1,0,2)
    Permutation(1,2,0).inverse is
      Permutation(2,0,1)

  test("Permutations of a given degree can be enumerated"):
    Permutation.enumerate(degree = 1) is Set(
      Permutation.identity(1)
    )
    Permutation.enumerate(degree = 3) is Set(
      Permutation(0, 1, 2), Permutation(0, 2, 1),
      Permutation(1, 0, 2), Permutation(1, 2, 0),
      Permutation(2, 0, 1), Permutation(2, 1, 0)
    )
    
  test("Permutations of a given degree can be made into a group"):
    val group: Group[Permutation] =
      Permutation.symmetricGroup(degree = 4)
    GroupVerifier.checkGroupOf[Permutation](group)
    group.order is 24

  test("Permutations can be ordered"):
    Permutation(0, 1) <  Permutation(1, 0) is true
    Permutation(0, 1) >=  Permutation(1, 0) is false
    intercept[IllegalArgumentException]:
      Permutation(0, 1) <  Permutation(0)
    .getMessage is "cannot compare permutations of different degrees"
    val group: Group[Permutation] =
      Permutation.symmetricGroup(degree = 3)
    group.unit.isInstanceOf[Comparable[Permutation]] is true
    group.elements.toSeq.sorted is Seq(
      Permutation(0,1,2), Permutation(0,2,1),
      Permutation(1,0,2), Permutation(1,2,0),
      Permutation(2,0,1), Permutation(2,1,0)
    )

  test("Permutation parity"):
    Permutation.identity(3).parity is 1
    Permutation(1,0).parity is -1
    Permutation(1,2,0).parity is 1
    Permutation(2,0,1).parity is 1
    Permutation(2,0,3,1).parity is -1

  test("Permutation to string"):
    Permutation.identity(3).toString is "Permutation(0, 1, 2)"
    Permutation(2,0,1).toString is "Permutation(2, 0, 1)"
    Permutation(2, 0, 3, 1).toString is "Permutation(2, 0, 3, 1)"

  test("Generating permutation groups"):
    def checkGroup(
      expectedOrder: Int,
      generators: Permutation*
    ): Unit =
      val group: Group[Permutation] =
        Permutation.group(generators*)
      GroupVerifier.checkGroupOf[Permutation](group)
      group.order is expectedOrder
    checkGroup(1)
    checkGroup(2, Permutation(0, 2, 1))
    checkGroup(6, Permutation(0, 2, 1), Permutation(2, 0, 1))
    checkGroup(6, Permutation(0, 2, 1), Permutation(2, 0, 1))
    checkGroup(48, Permutation(1, 2, 3, 4, 5, 6, 7, 0), Permutation(7, 1, 0, 6, 3, 5, 4, 2))


