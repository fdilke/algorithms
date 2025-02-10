package com.fdilke.algebra.permutation

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite
import GroupSugar._

class GroupSugarSpec extends FunSuite:
  given Group[Permutation] = Permutation.symmetricGroup(3)

  test("can multiply elements of a group"):
    Permutation(1,2,0) * Permutation(1,2,0) is Permutation(2,0,1)

  test("can calculate conjugates"):
    Permutation(1,2,0) ^ Permutation(0, 2, 1) is Permutation(2,0,1)

  test("can calculate unary inverse"):
    ~Permutation(1,2,0) is Permutation(2,0, 1)
    ~Permutation(0, 2, 1) is Permutation(0, 2, 1)

  test("can calculate element orders"):
    Permutation(0,1,2).order is 1
    Permutation(0,2,1).order is 2
    Permutation(1,2,0).order is 3

  test("can calculate canonical conjugates"):
    Permutation(0,1,2).canonicalConjugate is Permutation(0,1,2)
    Permutation(0,2,1).canonicalConjugate is Permutation(0,2,1)
    Permutation(2,1,0).canonicalConjugate is Permutation(0,2,1)
    Permutation(2,0,1).canonicalConjugate is Permutation(1,2,0)
    Permutation(1,2,0).canonicalConjugate is Permutation(1,2,0)
