package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

abstract class ColorGraphSpec extends FunSuite:
  test("Can color the empty graph (with 0 colors)"):
    ColorGraph(0, Seq()) is Seq()
    
  test("Can color a trivial graph with 1 vertex"):
    ColorGraph(1, Seq(Seq(false))) is Seq(0)
    
  test("Can color a disconnected graph with 2 vertexes"):
    ColorGraph(0, Seq(Seq(false, false), Seq(false, false))) is Seq(0, 0)
    
  test("Can color a graph with 2 joined vertexes"):
    ColorGraph(0, Seq(Seq(false, true), Seq(true, false))) is Seq(0, 1)
    


