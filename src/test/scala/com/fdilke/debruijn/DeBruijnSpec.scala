package com.fdilke.debruijn

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class DeBruijnSpec extends FunSuite:
  test("calculate trivial case (n=1, k=1)"):
    DeBruijn.string(n=1, k=1) is "0"
  test("calculate almost-trivial case (n=1, k=2)"):
    DeBruijn.string(n=1, k=2) is "01"
  test("calculate less trivial case"):
    DeBruijn.string(n=1, k=2) is "01"
  test("calculate case n = 2, k = 2"):
    DeBruijn.string(n=2, k=2) is "0011"
  test("calculate case n = 3, k = 2"):
    DeBruijn.string(n=3, k=2) is "00010111" 
  test("calculate case n = 2, k = 3"):
    DeBruijn.string(n=2, k=3) is "001021122" 
    
