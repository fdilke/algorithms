package com.fdilke.blocks

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class DivisibilityConditionsSpec extends FunSuite:
  test("the basics for triples"):
    DivisibilityConditions(1, 2, 3, 7) is true
    DivisibilityConditions(1, 2, 3, 8) is false
    DivisibilityConditions(1, 2, 3, 9) is true
    DivisibilityConditions(1, 2, 3, 10) is false
    DivisibilityConditions(1, 2, 3, 13) is true
    DivisibilityConditions(1, 2, 3, 14) is false
    DivisibilityConditions(1, 2, 3, 15) is true
