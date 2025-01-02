package com.fdilke.truchet

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class TruchetGridSpec extends FunSuite:

    private val grid: SquareHolder = TruchetGrid(3, 4)

    test("grid has indexed squares"):
        val squares: Seq[Square] = grid.squares
        squares.size is 12
        squares.map { _.index } is (0 until 12 : Seq[Int])

    test("grid squares are self describing, consistent with lookup"):
        val aSquare: Square = grid.lookup(2, 1)
        aSquare.xPosition is 2
        aSquare.yPosition is 1

    test("grid square coordinates wrap automatically"):
        val target = grid.lookup(0, 1)
        grid.lookup(3, 1) is target
        grid.lookup(-3, 1) is target
        grid.lookup(0, -3) is target
        grid.lookup(0, 5) is target

    test("can navigate grid squares"):
        val aSquare: Square = grid.lookup(0, 3)
        aSquare.left is grid.lookup(2, 3)
        aSquare.right is grid.lookup(1, 3)
        aSquare.up is grid.lookup(0, 2)
        aSquare.down is grid.lookup(0, 0)

