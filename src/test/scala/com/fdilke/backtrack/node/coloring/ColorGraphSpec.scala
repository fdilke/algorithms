package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*

import scala.annotation.targetName

class ColorGraphSpec extends FunSuite:
  
  private def checkColoring(
    numColors: Int,
    coloring: Option[Seq[Int]],
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    coloring match
      case None => fail("coloring not found")
      case Some(coloring) =>
        for
          i <- coloring.indices
          j <- 0 until i
        do
          if adjacencyTable(i)(j) && (coloring(i) == coloring(j)) then
            fail("adjacent vertices have same color")
        if coloring.distinct.size > numColors then
          fail("too many colors")
        if PartialColoring.fromColorsAndAdjacencies(
          coloring,
          adjacencyTable
        ).amalgamations.nonEmpty then
          fail("not a minimal coloring: an amalgamation is possible")

  //noinspection AccessorLikeMethodIsUnit
  private def canJustColor(
    numColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    if numColors > 0 && ColorGraph(numColors - 1, adjacencyTable).isDefined then
       fail("this many colors are not required")
    checkColoring(
      numColors,
      ColorGraph(numColors, adjacencyTable),
      adjacencyTable
    )
    
  //noinspection AccessorLikeMethodIsUnit
  @targetName("canJustColorWithUnpackedAdjacencyTable")
  private def canJustColor(
    numColors: Int,
    unpackedAdjacencyTable: Boolean*
  ): Unit =
    canJustColor(numColors, ColorGraph.packAdjacencyTable(unpackedAdjacencyTable))

  //noinspection AccessorLikeMethodIsUnit
  @targetName("canJustColorWithAdjacencyPairs")
  private def canJustColor(
    numColors: Int,
    adjacencyPairs: (Int, Int)*
  ): Unit =
    canJustColor(numColors, ColorGraph.adjacencyTableFromPairs(adjacencyPairs*))

  test("Can construct an adjacency table from pairs"):
    ColorGraph.adjacencyTableFromPairs() is Seq()
    ColorGraph.adjacencyTableFromPairs((0, 1)) is Seq(Seq(false, true), Seq(true, false))
    ColorGraph.adjacencyTableFromPairs((1, 2)) is Seq(
      Seq(false, false, false),
      Seq(false, false, true),
      Seq(false, true, false)
    )
    ColorGraph.adjacencyTableFromPairs((0, 1), (1, 2)) is Seq(
      Seq(false, true, false),
      Seq(true,  false, true),
      Seq(false, true, false)
    )
    ColorGraph.adjacencyTableFromPairs((0, 3), (1, 2)) is Seq(
      Seq(false, false, false, true),
      Seq(false, false, true, false),
      Seq(false, true, false, false),
      Seq(true,  false, false, false)
    )

  test("Reject graphs unless they're antireflexive & symmetric"):
    intercept[IllegalArgumentException]:
      ColorGraph(2, 1 -> 1)
    .getMessage is
      "adjacency table must be antireflexive: fail at 1"

    intercept[IllegalArgumentException]:
      ColorGraph(2,
        false, false,
        true, false
      )
    .getMessage is
      "adjacency table must be symmetric: fail at 0, 1"

  test("Can color the empty graph (with 0 colors)"):
    canJustColor(0, Seq[Seq[Boolean]]())

  test("Can color the empty graph (with 0 colors) by adjacencies"):
    canJustColor(0, Seq.empty[Boolean]*)

  test("Can color a trivial graph with 1 vertex"):
    canJustColor(1, Seq(Seq(false)))

  test("Can color a disconnected graph with 2 vertexes"):
    canJustColor(1, Seq(Seq(false, false), Seq(false, false)))
    
  test("Can color a graph with 2 joined vertexes"):
    canJustColor(2, Seq(Seq(false, true), Seq(true, false)))
    
  test("Can color a graph with 2 joined vertexes, using an unpacked adjacency table"):
    canJustColor(2, false, true, true, false)

  test("Can color a graph with 2 joined vertexes by adjacencies"):
    canJustColor(2, 0 -> 1)

  private def torus(
    width: Int,
    height: Int
  ): Seq[(Int, Int)] =
    def cellIndex(i: Int, j: Int): Int =
      (i % width) * height + (j % height)
    (for
      i <- 0 to width
      j <- 0 to height
    yield
      Seq(
        cellIndex(i, j) -> cellIndex(i + 1, j),
        cellIndex(i, j) -> cellIndex(i, j + 1)
      )
    ).flatten

  test("torus(2,2) requires 2 colors"):
    canJustColor(3, torus(2, 3)*)

  test("torus(2,3) requires 3 colors"):
    canJustColor(3, torus(2, 3)*)

  test("torus(3,3) requires 3 colors"):
    canJustColor(3, torus(3, 3)*)

  test("torus(5,2) requires 3 colors"):
    canJustColor(3, torus(5, 2)*)

//  test("torus(2,5) requires 3 colors"):
//    canJustColor(3, torus(2, 5)*)

//  test("torus(3,4) requires 3 colors"):
//    canJustColor(3, torus(3, 4)*)
//
//  test("torus(4, 3) requires 3 colors"):
//    canJustColor(3, torus(4, 3)*)
//
//  test("torus(4, 4) requires 2 colors"):
//    canJustColor(3, torus(4, 3)*)


