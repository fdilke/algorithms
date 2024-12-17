package com.fdilke.truchet

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class TruchetGridSpec extends FunSuite:

    private val grid = TruchetGrid(3, 2)

    test("grid has indexed squares"):
        val squares: Seq[Square] = grid.squares
        squares.size is 6
        squares.map { _.index }.sorted is (0 until 6 : Seq[Int])
