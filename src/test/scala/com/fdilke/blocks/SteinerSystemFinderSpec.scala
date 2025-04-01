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

  private def checkSteinerDefault(r: Int, q: Int, n: Int): Unit =
    SteinerSystemFinder(r=r, q=q) match
      case None => fail("could not construct system")
      case Some(steiner) =>
//        println("blocks are:\n" + steiner.blocks.mkString("\n"))
        steiner.r is r
        steiner.q is q
        steiner.n is n
        steiner.sanityTest()

  test("can find an S(2,3,7)"):
    checkSteiner(r=2,q=3,n=7)
    checkSteinerDefault(r=2,q=3,n=7)

  test("can find an S(2,3,9)"):
    checkSteiner(r=2,q=3,n=9)

//  test("can find an S(2,3,13)"):
//    checkSteiner(r=2,q=3,n=13)

  test("can find an S(2,4,13)"):
    checkSteiner(r=2,q=4,n=13)
    checkSteinerDefault(r=2,q=4,n=13)

  test("can find an S(2,5,21)"):
    checkSteiner(r=2,q=5,n=21)
    checkSteinerDefault(r=2,q=5,n=21)

//  test("can find an S(3,5,17)"):
//    checkSteiner(r=3,q=5,n=17)
//  test("can find an S(3,5,26)"):
//    checkSteiner(r=3,q=5,n=26)
//  test("can find an S(3,5,41)"):
//    checkSteiner(r=3,q=5,n=41)

  test("can find an S(3,4,8)"):
    checkSteiner(r=3,q=4,n=8)
    checkSteinerDefault(r=3,q=4,n=8)

//  test("can find an S(3,4,10)"):
//    checkSteiner(r=3,q=4,n=10)

//  test("can find an S(4,5,11)"):
//    checkSteiner(r=4,q=5,n=11)

  test("can find an S(3,6,22)"):
    checkSteiner(r=3,q=6,n=22)
    checkSteinerDefault(r=3,q=6,n=22)

//  test("can find an S(5,6,12)"):
//    checkSteiner(r=5,q=6,n=12)

//object Find2_3_13 extends App:
//  SteinerSystemFinder(r=2, q=3, n=13) match
//    case None => throw new IllegalArgumentException("not found")
//    case Some(steiner) =>
//      println(s"found a solution with ${steiner.blocks.size} blocks")