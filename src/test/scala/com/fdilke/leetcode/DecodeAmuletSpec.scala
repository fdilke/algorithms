package com.fdilke.leetcode

import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._
import munit.FunSuite

class DecodeAmuletSpec extends FunSuite:

  extension[A] (cycle: Seq[A])
    def isCycle(altCycle: A*): Unit =
      sameCycle[A](cycle, altCycle) is true

  test("can decode amulets"):
    DecodeAmulet[Int]().isCycle()
    DecodeAmulet[Int](
      (1, 2)
    ) isCycle(1, 2)
    DecodeAmulet[String](
      ("mo", "eeny"),
      ("meeny", "miny"),
      ("eeny", "meeny"),
      ("miny", "mo")
    ) isCycle(
      "eeny", "meeny", "miny", "mo"
    )
    DecodeAmulet[Boolean](
      (false, true)
    ) isCycle(
      true, false
    )
    DecodeAmulet[Int](
      (1, 2)
    ) isCycle(1, 2)

  test("can detect faulty amulets"):
    intercept[IllegalArgumentException]:
      DecodeAmulet[Int](
        (1, 2), (2, 3)
      )
    intercept[IllegalArgumentException]:
      DecodeAmulet[Int](
        (1, 1)
      )
