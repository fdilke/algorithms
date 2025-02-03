package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

import scala.annotation.targetName
import Graph.{oddGraph, torus}

class ColorGraphJoinSpec extends ColorGraphSpec(ColorGraphByJoins)

class ColorGraphLoopSpec extends ColorGraphSpec(ColorGraphLoop)

class ColorGraphTweakedLoopSpec extends ColorGraphSpec(ColorGraphTweakedLoop)
 
class ColorGraphSpec(
  algo: ColoringAlgo
) extends FunSuite:
  
  private def checkColoring(
    numColors: Int,
    checkMinimal: Boolean,
    coloring: Option[Seq[Int]],
    graph: Graph
  ): Unit =
    coloring match
      case None => fail("coloring not found")
      case Some(coloring) =>
        for
          i <- coloring.indices
          j <- 0 until i
        do
          if graph.adjacencyTable(i)(j) && (coloring(i) == coloring(j)) then
            fail("adjacent vertices have same color")
        if coloring.distinct.size > numColors then
          fail("too many colors")
        if checkMinimal && PartialColoring.fromColorsAndGraph(
          coloring,
          graph
        ).amalgamations.nonEmpty then
          fail("not a minimal coloring: an amalgamation is possible")

  //noinspection AccessorLikeMethodIsUnit
  private def canColor(
    numColors: Int,
    checkMinimal: Boolean,
    graph: Graph
  ): Unit =
    if checkMinimal && numColors > 0 && algo(numColors - 1, graph).isDefined then
       fail("this many colors are not required")
    checkColoring(
      numColors,
      checkMinimal,
      algo(numColors, graph),
      graph
    )

  //noinspection AccessorLikeMethodIsUnit
  @targetName("canJustColorWithUnpackedAdjacencyTable")
  private def canColor(
    numColors: Int,
    checkMinimal: Boolean,
    unpackedAdjacencyTable: Boolean*
  ): Unit =
    canColor(numColors, checkMinimal, Graph(unpackedAdjacencyTable*))

  //noinspection AccessorLikeMethodIsUnit
  @targetName("canJustColorWithAdjacencyPairs")
  private def canColor(
    numColors: Int,
    checkMinimal: Boolean,
    adjacencyPairs: (Int, Int)*
  ): Unit =
    canColor(numColors, checkMinimal, Graph(adjacencyPairs*))

  test("Reject graphs unless they're antireflexive & symmetric"):
    intercept[IllegalArgumentException]:
      algo(2, Graph(1 -> 1))
    .getMessage.startsWith("adjacency table must be antireflexive: fail at") is true

    intercept[IllegalArgumentException]:
      algo(2, Graph(
        false, false,
        true, false
      ))
    .getMessage is
      "adjacency table must be symmetric: fail at 0, 1"

  test("Can color the empty graph (with 0 colors)"):
    canColor(0, true, Graph(Seq.empty[Boolean] *))

  test("Can color the empty graph (with 0 colors) by adjacencies"):
    canColor(0, true, Graph(Seq.empty[(Int, Int)] *))

  test("Can color a trivial graph with 1 vertex"):
    canColor(1, true, Graph(false))

  test("Can color a disconnected graph with 2 vertexes"):
    canColor(1, true, Graph(false, false, false, false))
    
  test("Can color a graph with 2 joined vertexes"):
    canColor(2, true, Graph(false, true, true, false))
    
  test("Can color a graph with 2 joined vertexes, using an unpacked adjacency table"):
    canColor(2, true, false, true, true, false)

  test("Can color a graph with 2 joined vertexes by adjacencies"):
    canColor(2, true, 0 -> 1)

  test("chi(torus(2,2)) == 3"):
    canColor(2, true, torus(2, 2))

  test("chi(torus(2,3)) == 3"):
    canColor(3, true, torus(2, 3))

  test("chi(torus(3,2)) == 3"):
    canColor(3, true, torus(3, 2))

  test("chi(torus(3,3)) == 3"):
    canColor(3, true, torus(3, 3))

// too slow: 3.2 sec
  test("chi(Petersen) <= 3"):
    val petersen = oddGraph(3)
    canColor(3, false, petersen)

// too slow - ~5sec */
    
  test("chi(torus(5,2)) <= 3"):
    canColor(3, false, torus(5, 2))

  test("chi(torus(2,5)) <= 3"):
    canColor(3, false, torus(2, 5))

/* MUCH too slow :( */
    
  test("chi(torus(3,4)) <= 3"):
    canColor(3, false, torus(3, 4))

  test("chi(torus(4, 3)) <= 3"):
    canColor(3, false, torus(4, 3))

  test("chi(torus(4, 4)) <= 3"):
    canColor(3, false, torus(4, 3))

  test("can quickly color a graph with enough colors"):
    canColor(25, false, torus(5, 5))
//    canColor(100, false, torus(10, 10)*)


