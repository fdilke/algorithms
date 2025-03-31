package com.fdilke.blocks

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*
import com.fdilke.utility.SetsUtilities.showSequence

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

  test("the (broken!) residue-based divisibility conditions are satisfiable for 2 <= r < q <= 10, apart from the known (fake) blind spot at (7,9)"):
    for
      q <- 3 to 10
      r <- 2 until q
    do
      val (modulus: Int, residues: Seq[Int]) =
        DivisibilityConditions.onN(lambda = 1, r = r, q = q)
      // println(s"r = $r, q = $q, modulus = $modulus")
      residues.isEmpty is:
        Set((7, 9), (8, 10)) contains (r, q)

  test("alternate check that the divisibility conditions are satisfiable for 2 <= r < q <= 20"):
    for
      q <- 3 to 20
      r <- 2 until q
    do
      // print(s"q=$q, r=$r ... ")
      val n: Int =
        LazyList.iterate(q + 1)(_ + 1).find: n =>
          DivisibilityConditions(lambda = 1, r=r, q=q, n=n)
        .get
      // println(s"n=$n")

  test("calculate the least N for a given lambda, r, q"):
    DivisibilityConditions.leastN(1, 2, 3) is 7
    DivisibilityConditions.leastN(1, 2, 4) is 13
    DivisibilityConditions.leastN(1, 3, 4) is 8

  test("calculate all N for a given lambda, r, q"):
    DivisibilityConditions.allN(1, 2, 3).take(3) is Seq(7, 9, 13)
    DivisibilityConditions.allN(1, 2, 4).take(4) is Seq(13, 16, 25, 28)
    DivisibilityConditions.allN(1, 2, 5).take(4) is Seq(21, 25, 41, 45)
    DivisibilityConditions.allN(1, 3, 4).take(5) is Seq(8, 10, 14, 16, 20)
    DivisibilityConditions.allN(1, 3, 5).take(6) is Seq(17, 26, 41, 50, 62, 65)
    DivisibilityConditions.allN(1, 4, 5).take(6) is Seq(11, 15, 17, 21, 23, 27)

