package com.fdilke.blocks

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite

class SteinerSystemFinderSpec extends FunSuite:
  private def checkSteiner(r: Int, q: Int, n: Int): Unit =
    SteinerSystemFinder(r=r, q=q, n=n) match
      case None => fail("could not construct system")
      case Some(steiner) =>
//        println("blocks are:\n" + steiner.blocks.mkString("\n"))
        steiner.r is r
        steiner.q is q
        steiner.n is n
        steiner.sanityTest()

  test("can find an S(2,3,7)"):
    checkSteiner(r=2,q=3,n=7)

  test("can find an S(2,4,13)"):
    checkSteiner(r=2,q=4,n=13)

  test("can find an S(3,4,8)"):
    checkSteiner(r=3,q=4,n=8)
