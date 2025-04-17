package com.fdilke.leetcode

import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._
import munit.FunSuite

class DecodeAmuletSpec extends FunSuite:

  extension[A] (cycle: Seq[A])
    def isCycle(altCycle: A*): Unit =
      println(s"comparing $cycle , $altCycle")
      sameCycle[A](cycle, altCycle) is true

  test("can decode simple amulets"):
    DecodeAmulet[Int]().isCycle()
    DecodeAmulet[Int](
      (1, 2)
    ) isCycle(1, 2)
