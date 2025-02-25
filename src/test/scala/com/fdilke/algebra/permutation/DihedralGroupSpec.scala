package com.fdilke.algebra.permutation

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class DihedralGroupSpec extends FunSuite:
  test("The dihedral group makes sense only for a positive number of elements"):
    intercept[IllegalArgumentException]:
      DihedralGroup(-2)
    intercept[IllegalArgumentException]:
      DihedralGroup(0)

  test("DG makes sense only for an even number of elements"):
    intercept[IllegalArgumentException] {
      DihedralGroup(7)
    }

  test("DG should actually be a group"):
    GroupVerifier.checkGroupOf[DihedralSymmetry](DihedralGroup(4))

  test("DG should have the right order"):
    DihedralGroup(2).order is 2
    DihedralGroup(8).order is 8
