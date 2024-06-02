package com.fdilke.debruijn

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._

class GrittyDeBruijnSpec extends DeBruijnSpec(GrittyDeBruijn)

trait DeBruijnSpec(
  deBruijn: DeBruijn
) extends FunSuite:
  test("calculate trivial case (n=1, k=1)"):
    deBruijn.string(n=1, k=1) is "0"
  test("calculate almost-trivial case (n=1, k=2)"):
    deBruijn.string(n=1, k=2) is "01"
  test("calculate less trivial case"):
    deBruijn.string(n=1, k=2) is "01"
  test("calculate case n = 2, k = 2"):
    deBruijn.string(n=2, k=2) is "0011"
  test("calculate case n = 3, k = 2"):
    deBruijn.string(n=3, k=2) is "00010111" 
  test("calculate case n = 2, k = 3"):
    deBruijn.string(n=2, k=3) is "001021122" 
  test("more advanced case: n = 3, k = 3"):
   testDeBruijn(3, 3)
  test("yet more advanced cases: (3,4) and (4,3)"):
   testDeBruijn(3, 4)
   testDeBruijn(4, 3)

  def testDeBruijn(n: Int, k: Int): Unit =
    val deB: Seq[Int] = deBruijn(n, k)
    val length: Int = deB.size
    length is naivePow(n, k)
    val seqs: Set[Seq[Int]] =
      (0 until length).toSet.map: i =>
        deB.slice(i, i + n)
    seqs.size is length
