package com.fdilke.chain

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class ChainSpec extends FunSuite:
  given ChainRunner = Chain.VanillaRunner

  test("can create and run a pure chain of 2"):
    Chain.pure(2).run() is 2



