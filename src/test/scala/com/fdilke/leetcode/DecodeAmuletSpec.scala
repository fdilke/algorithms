package com.fdilke.leetcode

import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._
import munit.FunSuite

class DecodeAmuletSpec extends FunSuite:

  extension[A] (cycle: Seq[A])
    def isCycle(altCycle: Seq[A]): Unit =
      sameCycle[A](cycle, altCycle) is true
        
//  test("testing the tests"):
//    Seq.empty[Int] isCycle Seq.empty[Int]
//    Seq(1,3) isCycle Seq(1,3)
//    Seq(1,3) isCycle Seq(3,1)
//    Seq(1,4,7) isCycle Seq(4,7,1)
//    intercept[Exception]:
//      Seq(1,4,7) isCycle Seq(4,1,7)
//    intercept[Exception]:
//      Seq(1,2) isCycle Seq.empty[Int]
    
