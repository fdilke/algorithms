package com.fdilke.debruijn

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class DeBruijnSpec extends FunSuite:
  // test("calculate trivial case (n=1, k=1)"):
  //   DeBruijn(n=1, k=1) is Seq.empty
  test("dummy test for output"):
    for { p <- 1 until 7 }
      println(s"p=$p => ${ DeBruijn(n=p, k=2).mkString }")
    println(s"math thing 2^3 = ${ Math.pow(2, 3).toInt }")
    println(s"GDG(3, 2, xx) = ${ GFG.deBruijn(3, 2, "oi") }")
    1 is 1
  test("calculate less trivial case"):
    DeBruijn(n=1, k=2) is Seq(1, 0)
  test("calculate case n = 2"):
    DeBruijn(n=2, k=2) is Seq(0, 1, 1, 0, 0)
  test("calculate case n = 3"):
    DeBruijn(n=3, k=2) is Seq(0, 0, 1, 1, 1, 0, 1, 0, 0, 0)
    
    // 0011101000