package com.fdilke.truchet

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class TruchetGridSpec extends FunSuite:

    private val grid: SquareHolder = TruchetGrid(3, 2)

    test("grid has indexed squares"):
        val squares: Seq[Square] = grid.squares
        squares.size is 6
        squares.map { _.index } is (0 until 6 : Seq[Int])

    test("grid squares are self describing, consistent with lookup"):
        val aSquare = grid.lookup(2, 1)
        aSquare.xPosition is 2
        aSquare.yPosition is 1
