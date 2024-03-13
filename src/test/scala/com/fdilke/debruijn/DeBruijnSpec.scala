package com.fdilke.debruijn

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class DeBruijnSpec extends FunSuite:
  test("calculate trivial case (n=1, k=1)"):
    DeBruijn(n=1, k=1) is Seq.empty
    