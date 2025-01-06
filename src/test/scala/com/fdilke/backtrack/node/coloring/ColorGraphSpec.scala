package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class ColorGraphSpec extends FunSuite:
  test("Can color the empty graph (with 0 colors)"):
    ColorGraph(0, Seq()) is Seq()
    


