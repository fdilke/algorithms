package com.fdilke.algebra.permutation

import munit.FunSuite

import com.fdilke.utility.RichFunSuite._

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
      Permutation.group(degree = 4)
    GroupVerifier.checkGroupOf[Permutation](group)
    group.order is 24
