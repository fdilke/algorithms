package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class PartialColoringSpec extends FunSuite:
  private val sampleGraph: PartialColoring =
    PartialColoring(
      numVertices = 5,
      adjacencies = Seq(
        Seq(false, true,  false,  false, false),
        Seq(true,  false, true,   false, false),
        Seq(false, true,  false,  true,  false),
        Seq(false, false, true,   false, true),
        Seq(false, false, false,  true,  false)
      ),
      colors = Seq(0,1,0,3,4),
      colorAdjacencies = Seq(
        Seq(false, true,  true,  true,  false),
        Seq(true,  false, true,  false, false),
        Seq(true, true,   true,  true,  true),   // 2 washed out ; have it be adjacent to everything. Also column #2
        Seq(true,  false, true,  false, true),
        Seq(false, false, true,  true,  false)
      )
    )

  // 0---1---2---3---4 with 0,2 identified ; so colors 0,1,0,3,4 => 0---1---0---3---4

  test("Can identify the distinct colors"):
    sampleGraph.distinctColors is Seq(0,1,3,4)

  test("Can enumerate possible amalgamations"):
    sampleGraph.amalgamations is Seq(
      (0, 4), (1, 3), (1, 4)
    )
    
