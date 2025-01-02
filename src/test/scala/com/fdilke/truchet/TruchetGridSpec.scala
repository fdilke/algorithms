package com.fdilke.truchet

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class TruchetGridSpec extends FunSuite:

    private val squareGrid: SquareHolder = TruchetGrid(3, 4, false)
    private val torusGrid: SquareHolder = TruchetGrid(3, 4, true)
    private val grids = Seq(squareGrid, torusGrid)

    test("grid has indexed squares"):
        for
            grid <- grids
        do
            val squares: Seq[Square] = grid.squares
            squares.size is 12
            squares.map { _.index } is (0 until 12 : Seq[Int])

    test("grid squares are self describing, consistent with lookup"):
        for
            grid <- grids
        do
            val aSquare: Square = grid.lookup(2, 1)
            aSquare.xPosition is 2
            aSquare.yPosition is 1

    test("grid square coordinates wrap automatically"):
        for
            grid <- grids
        do
            val target = grid.lookup(0, 1)
            grid.lookup(3, 1) is target
            grid.lookup(-3, 1) is target
            grid.lookup(0, -3) is target
            grid.lookup(0, 5) is target

    test("torus: can navigate grid squares"):
        val aSquare: Square = torusGrid.lookup(0, 3)
        aSquare.left is Some(torusGrid.lookup(2, 3))
        aSquare.right is Some(torusGrid.lookup(1, 3))
        aSquare.up is Some(torusGrid.lookup(0, 2))
        aSquare.down is Some(torusGrid.lookup(0, 0))

    test("square: can navigate grid squares"):
        val aSquare: Square = squareGrid.lookup(0, 3)
        aSquare.left is None
        aSquare.right is Some(squareGrid.lookup(1, 3))
        aSquare.up is Some(squareGrid.lookup(0, 2))
        aSquare.down is None

