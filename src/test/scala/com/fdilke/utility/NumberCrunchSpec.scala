package com.fdilke.utility

import RichFunSuite._
import munit.FunSuite
import NumberCrunch._

class NumberCrunchSpec extends FunSuite:
    test("Does more convenient modulus calculation"):
        confineTo(0, 7) is 0
        confineTo(7, 7) is 0
        confineTo(-7, 7) is 0
        confineTo(2, 7) is 2
        confineTo(-2, 7) is 5
        confineTo(9, 7) is 2
        confineTo(-9, 7) is 5

