package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*

import scala.annotation.targetName

abstract class ColorGraphSpec extends FunSuite:
  
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
          if (adjacencyTable(i)(j) && (coloring(i) == coloring(j)))
            fail("adjacent vertices have same color")
        if (coloring.distinct.length > numColors)
          fail("too many colors")

  private def canColor(
    numColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    checkColoring(numColors, ColorGraph(numColors, adjacencyTable), adjacencyTable)

  @targetName("canColorWithAdjacencyPairs")
  private def canColor(
    numColors: Int,
    adjacencyPairs: (Int, Int)*
  ): Unit =
    canColor(numColors, ColorGraph.adjacencyTableFromPairs(adjacencyPairs*))

  test("Can construct an adjacency table from pairs"):
    ColorGraph.adjacencyTableFromPairs() is Seq()
    ColorGraph.adjacencyTableFromPairs((0, 1)) is Seq(Seq(false, true), Seq(true, false))
    ColorGraph.adjacencyTableFromPairs((1, 2)) is Seq(
      Seq(false, false, false),
      Seq(false, false, true),
      Seq(false, true, false)
    )
    ColorGraph.adjacencyTableFromPairs((0, 1), (1, 2)) is Seq(
      Seq(false, true, true),
      Seq(true,  false, true),
      Seq(true, true, false)
    )
    ColorGraph.adjacencyTableFromPairs((0, 3), (1, 2)) is Seq(
      Seq(false, false, false, true),
      Seq(false, false, true, false),
      Seq(false, true, false, false),
      Seq(true,  false, false, false)
    )

  test("Can color the empty graph (with 0 colors)"):
    canColor(0, Seq[Seq[Boolean]]())

//  test("Can color the empty graph (with 0 colors) by adjacencies"):
//    canColor(0)

  test("Can color a trivial graph with 1 vertex"):
    canColor(1, Seq(Seq(false)))

  test("Can color a disconnected graph with 2 vertexes"):
    canColor(1, Seq(Seq(false, false), Seq(false, false)))
    
  test("Can color a graph with 2 joined vertexes"):
    canColor(2, Seq(Seq(false, true), Seq(true, false)))

  test("Can color a graph with 2 joined vertexes by adjacencies"):
    canColor(1, 0 -> 1)

