package com.fdilke.leetcode

import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._
import munit.FunSuite

class DecodeAmuletSpec extends FunSuite:

  extension[A] (cycle: Seq[A])
    infix def checkCycle(altCycle: A*): Unit =
      sameCycle[A](cycle, altCycle) is true

  test("can decode amulets"):
    DecodeAmulet[Int]().checkCycle()
    DecodeAmulet[Int](
      (1, 2)
    ) checkCycle(1, 2)
    DecodeAmulet[String](
      ("mo", "eeny"),
      ("meeny", "miny"),
      ("eeny", "meeny"),
      ("miny", "mo")
    ) checkCycle(
      "eeny", "meeny", "miny", "mo"
    )
    DecodeAmulet[Boolean](
      (false, true)
    ) checkCycle(
      true, false
    )
    DecodeAmulet[Int](
      (1, 2)
    ) checkCycle(1, 2)

  test("can detect faulty amulets"):
    intercept[IllegalArgumentException]:
      DecodeAmulet[Int](
        (1, 2), (2, 3)
      )
    intercept[IllegalArgumentException]:
      DecodeAmulet[Int](
        (1, 1)
      )
