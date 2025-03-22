package com.fdilke.utility

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*
import com.fdilke.utility.SetsUtilities.{bitCount, bits}

import scala.runtime.Arrays

class RisingIntsSpec extends FunSuite:

  test("semantics of equality"):
    val rising12 = RisingInts(Array[Int](1, 2))
    val rising12b = RisingInts(Array[Int](1, 2))
    val rising23 = RisingInts(Array[Int](2, 3))
    rising12 is rising12
    rising12.array.sameElements(rising12b.array) is true
    rising12 is rising12b
    (rising12 == rising23) is false

  private def nextSequence(
    rising: Int*
  ): Seq[Int] =
    RisingInts(rising).next.array

  test("nextSequence"):
    intercept[IllegalArgumentException]:
      nextSequence()
    nextSequence(0) is Seq(1)
    nextSequence(1) is Seq(2)
    nextSequence(0, 1) is Seq(0, 2)
    nextSequence(0, 2) is Seq(1, 2)
    nextSequence(1, 2) is Seq(0, 3)
    nextSequence(0, 1, 2) is Seq(0, 1, 3)
    nextSequence(0, 1, 2, 4) is Seq(0, 1, 3, 4)
    nextSequence(1, 2, 3, 5) is Seq(0, 1, 4, 5)
    val bitSequences: Seq[Seq[Int]] =
      for
        i <- 0 to 1 << 8 if bitCount(i) == 4
      yield
        bits(i)
    bitSequences.size is (8 * 7 * 6 * 5)/(1 * 2 * 3 * 4)
    for
      j <- 0 until (bitSequences.size - 1)
    do
      bitSequences(j + 1) is nextSequence(bitSequences(j)*)

