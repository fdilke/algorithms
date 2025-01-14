package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

abstract class ColorGraphSpec extends FunSuite:
  
  private def canColor(
    numColors: Int,
    adjacencies: Seq[Seq[Boolean]]
  ): Unit =
    ColorGraph(numColors, adjacencies) match
      case None => fail("coloring not found")
      case Some(coloring) =>
        val numVertices = adjacencies.length
        for
          i <- 0 until numVertices
          j <- 0 until i
        do
          if (adjacencies(i)(j) && (coloring(i) == coloring(j)))
            fail("adjacent vertices have same color")
        if (coloring.distinct.length > numColors)
          fail("too many colors")
          
    
  test("Can color the empty graph (with 0 colors)"):
    canColor(0, Seq())
    
  test("Can color a trivial graph with 1 vertex"):
    canColor(1, Seq(Seq(false)))
    
  test("Can color a disconnected graph with 2 vertexes"):
    canColor(1, Seq(Seq(false, false), Seq(false, false)))
    
  test("Can color a graph with 2 joined vertexes"):
    canColor(2, Seq(Seq(false, true), Seq(true, false)))
