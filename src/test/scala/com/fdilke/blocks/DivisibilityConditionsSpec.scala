package com.fdilke.blocks

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class DivisibilityConditionsSpec extends FunSuite:
  test("the basics for triples"):
    DivisibilityConditions(1, 2, 3, 7) is true
    DivisibilityConditions(1, 2, 3, 8) is false
    DivisibilityConditions(1, 2, 3, 9) is true
    DivisibilityConditions(1, 2, 3, 10) is false
    DivisibilityConditions(1, 2, 3, 13) is true
    DivisibilityConditions(1, 2, 3, 14) is false
    DivisibilityConditions(1, 2, 3, 15) is true

  test("criteria on n for given L,r,q"):
    DivisibilityConditions.onN(1, 2, 3) is
      (6, Seq(1, 3))
    val (modulus, residues): (Int, Seq[Int]) =
      DivisibilityConditions.onN(1, 4, 5) 
    modulus is 60
    residues is Seq(
      3, 5, 11, 15, 17, 21, 
      23, 27, 33, 35, 41, 45, 
      47, 51, 53, 57
    )
    for
      n <- 0 until 1000
    do
      DivisibilityConditions(1, 4, 5, n) is
        residues.contains(n % modulus)
