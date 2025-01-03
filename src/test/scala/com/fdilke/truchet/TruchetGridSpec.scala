package com.fdilke.truchet

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite
import Orientation._
import scala.util.Random

class TruchetGridSpec extends FunSuite:

  private val squareGrid: SquareHolder & TileHolder = TruchetGrid(3, 4, false, new Random(0L))
  private val torusGrid: SquareHolder & TileHolder = TruchetGrid(3, 4, true, new Random(0L))
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

  test("grid has predictable orientations"):
    for
      grid <- grids
    do
      grid.lookup(0, 0).orientation is Forward
      grid.lookup(0, 1).orientation is Forward
      grid.lookup(1, 0).orientation is Forward
      grid.lookup(1, 1).orientation is Backward

  test("grid has indexed tiles"):
    for
      grid <- grids
    do
      val tiles: Seq[Tile] = grid.tiles
      tiles.size is 24
      tiles.map {
        _.index
      } is (0 until 24: Seq[Int])

  test("grid prints to expected pattern"):
    for
      grid <- grids
    do
      val showGrid = grid.toString
      //            println(s"grid is:\n${showGrid}\n")
      showGrid is "///\n/\\/\n\\/\\\n/\\\\\n"

  test("grids model leftTile and upTile"):
    for
      grid <- grids
    do
      val square00: Square = grid.lookup(0, 0)
      square00.leftTile is square00.tiles(0)
      square00.upTile is square00.tiles(0)
      val square11: Square = grid.lookup(1, 1)
      square11.leftTile is square11.tiles(1)
      square11.upTile is square11.tiles(0)
      val square12: Square = grid.lookup(1, 2)
      square12.leftTile is square12.tiles(0)
      square12.upTile is square12.tiles(0)
      val square22: Square = grid.lookup(2, 2)
      square22.leftTile is square22.tiles(1)
      square22.upTile is square22.tiles(0)

  /*
      ///
      /\/
      \/\
      /\\

      ./././
      /././.
      ./\../
      /..\/.
      \../\.
      .\/..\
      ./\.\.
      /..\.\

      0/8/6/
      /1/9/7
      2/\08/
      /31\/9
      \42/\0
      5\/31\
      6/\4\2
      /75\3\
*/

  test("square grid models adjacencies"):
    val mappedAdjs = squareGrid.tileAdjacencies.map:
      case (t, u) => (t.index, u.index)
    mappedAdjs is Seq(
      (1,8), (1,2), (3,11), (3,4), (4,12), (5,6), (7,15), (9,16), (9,10),
      (10,18), (11,12), (13,21), (13,14), (14,23), (17,18), (19,20), (21,22)
    )

  test("toroidal grid models adjacencies"):
    val mappedAdjs = torusGrid.tileAdjacencies.map:
      case (t, u) => (t.index, u.index)
    mappedAdjs is Seq(
      (1,8), (1,2), (3,11), (3,4), (4,12), (5,6), (7,15), (7,0), (9,16), (9,10),
      (10,18), (11,12), (13,21), (13,14), (14,23), (15,8), (17,0), (17,18), (19,2),
      (19,20), (20,5), (21,22), (22,6), (23,16)
    )

  test("square grid models regions"):
    squareGrid.regions is Seq(
      0, 1,   1, 2,     2, 3,
      3, 4,   1, 5,     5, 2,
      2, 6,   6, 4,     5, 5,
      5, 7,   7, 6,     6, 6
    )

  test("toroidal grid models regions"):
    torusGrid.regions is Seq(
      0, 0,     0, 1,       1, 0,
      0, 0,     0, 0,       0, 1,
      1, 0,     0, 0,       0, 0,
      0, 0,     0, 0,       0, 0
    )

//    test("square grid models regions"):
//      squareGrid.regions is Seq(
//          0, 2,     2, 12,      12, 6,
//          6, 15,    2, 18,      18, 12,
//          12, 22,   22, 15,     18,18,
//          18, 20,   20, 22,     22, 22
//      )
//
//    test("toroidal grid models regions"):
//      torusGrid.regions is Seq(
//          6, 6,     6, 12,      12, 6,
//          6, 6,     6, 6,       6, 12,
//          12, 6,    6, 6,       6, 6,
//          6, 6,     6, 6,       6, 6
//      )
