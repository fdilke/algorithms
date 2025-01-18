package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class PartialColoringSpec extends FunSuite:
  private val sampleGraph: PartialColoring =
    PartialColoring(
      colors = Seq(0,1,0,3,4),
      colorAdjacencies = Seq(
        Seq(false, true,  true,  true,  false),
        Seq(true,  false, true,  false, false),
        Seq(true, true,   true,  true,  true),   // 2 washed out ; have it be adjacent to everything. Also column #2
        Seq(true,  false, true,  false, true),
        Seq(false, false, true,  true,  false)
      )
    )

  private val noAmalgamationsGraph: PartialColoring =
    PartialColoring(
      colors = Seq(0,1,1),
      colorAdjacencies = Seq(
        Seq(false, true,  true),
        Seq(true,  false, true),
        Seq(true, true,   true),
      )
    )

  // 0---1---2---3---4 with 0,2 identified ; so colors 0,1,0,3,4 => 0---1---0---3---4

  test("Can identify the distinct colors"):
    sampleGraph.distinctColors is Seq(0,1,3,4)

  test("Can enumerate possible amalgamations"):
    sampleGraph.amalgamations is Seq(
      (0, 4), (1, 3), (1, 4)
    )

  // amalgamate 0 and 4 in 0---1---0---3---4 => 0---1---0---3---0

  private val amalgamate04Graph: PartialColoring =
    PartialColoring(
      colors = Seq(0, 1, 0, 3, 0),
      colorAdjacencies = Seq(
        Seq(false, true, true, true,  true),
        Seq(true, false, true, false, true),
        Seq(true, true, true, true,   true), // 2 washed out ; have it be adjacent to everything. Also column #2
        Seq(true, false, true, false, true),
        Seq(true, true, true, true,   true) // 4 washed out
      )
    )

  test("Can apply an amalgamation"):
    sampleGraph.amalgamate(0 -> 4) is amalgamate04Graph

  // amalgamate 1 and 3 in 0---1---0---3---4 => 0---1---0---1---4

  private val amalgamate13Graph: PartialColoring =
    PartialColoring(
      colors = Seq(0,1,0,1,4),
      colorAdjacencies = Seq(
        Seq(false, true, true, true,  false),
        Seq(true, false, true, true,  true),
        Seq(true, true, true, true,   true), // 2 washed out ; have it be adjacent to everything. Also column #2
        Seq(true, true, true, true,   true), // 3 washed out
        Seq(false, true, true, true,  false)
      )
    )

  test("Can apply an amalgamation (2)"):
    sampleGraph.amalgamate(1 -> 3) is amalgamate13Graph

  // amalgamate 1 and 4 in 0---1---0---3---4 => 0---1---0---3---1

  private val amalgamate14Graph: PartialColoring =
    PartialColoring(
      colors = Seq(0,1,0,3,1),
      colorAdjacencies = Seq(
        Seq(false, true, true, true,  true),
        Seq(true, false, true, true,  true),
        Seq(true, true, true, true,   true), // 2 washed out ; have it be adjacent to everything. Also column #2
        Seq(true, true, true, false,  true),
        Seq(true, true, true, true,   true) // 4 washed out
      )
    )

  test("Can apply an amalgamation (3)"):
    sampleGraph.amalgamate(1 -> 4) is amalgamate14Graph

  test("Can check consistency for 'blank' colors"):
    sampleGraph.consistentBlanks is true
    noAmalgamationsGraph.consistentBlanks is true
    PartialColoring(
      colors = Seq(0, 1, 0, 3, 4),
      colorAdjacencies = Seq(
        Seq(false, true, true, true, false),
        Seq(true, false, true, false, false),
        Seq(true, true, true, false, true), // 2 washed out ; have it be adjacent to everything. Also column #2
        Seq(true, false, true, false, true),
        Seq(false, false, true, true, false)
      )
    ).consistentBlanks is false
    PartialColoring(
      colors = Seq(0, 1, 0, 3, 4),
      colorAdjacencies = Seq(
        Seq(false, true, true, true, false),
        Seq(true, false, true, false, false),
        Seq(true, true, true, true, true), // 2 washed out ; have it be adjacent to everything. Also column #2
        Seq(true, false, true, false, true),
        Seq(false, false, false, true, false)
      )
    ).consistentBlanks is false

  test("Can detect a graph with no amalgamations"):
    noAmalgamationsGraph.amalgamations is Iterable.empty

  test("can explore amalgamations via the NodeIterable API"):
    noAmalgamationsGraph.explore is
      Iterable(Right(noAmalgamationsGraph.colors))
    sampleGraph.explore is
      Iterable(
        Left(amalgamate04Graph),
        Left(amalgamate13Graph),
        Left(amalgamate14Graph)
      )

  test("can calculate color adjacencies from colors and adjacencies"):
    PartialColoring.fromColorsAndAdjacencies(
      Seq(1, 0, 2, 0),
      Seq(
        Seq(false, true, true, true),
        Seq(true, false, true, false),
        Seq(true, true, false, true),
        Seq(true, false, false, true)
      )
    ) is
      PartialColoring(
        colors = Seq(1, 0, 2, 0),
        colorAdjacencies = Seq(
          Seq(false, true, true, true),
          Seq(true, false, true, true),
          Seq(true, true, false, true),
          Seq(true, true, true, true)
        )
      )

  test("can calculate color adjacencies from colors and adjacencies (advanced)"):
    PartialColoring.fromColorsAndAdjacencies(
      Seq(2, 1, 3, 3, 2),
      Seq(
        Seq(false, true, true, false, false),
        Seq(true, false, false, true, true),
        Seq(true, false, false, false, true),
        Seq(false, true, false, false, true),
        Seq(false, true, true, true, false)
      )
    ) is
      PartialColoring(
        colors = Seq(2, 1, 3, 3, 2),
        colorAdjacencies = Seq(
          Seq(true, true, true, true, true),
          Seq(true, false, true, true, true),
          Seq(true, true, false, true, true),
          Seq(true, true, true, false, true),
          Seq(true, true, true, true, true)
        )
      )

  test("can calculate color adjacencies from colors and adjacencies (more advanced)"):
    PartialColoring.fromColorsAndAdjacencies(
      Seq(2, 0, 0, 2, 4, 0),
      Seq(
        Seq(false, true, true, false, false, true),
        Seq(true, false, false, true, false, false),
        Seq(true, false, false, true, true, false),
        Seq(false, true, false, false, true, true),
        Seq(false, false, true, true, false, true),
        Seq(true, false, false, true, true, false)
      )
    ) is
      PartialColoring(
        colors = Seq(2, 0, 0, 2, 4, 0),
        colorAdjacencies = Seq(
          Seq(false, true, true, true, true, true),
          Seq(true, true, true, true, true, true),
          Seq(true, true, false, true, true, true),
          Seq(true, true, true, true, true, true),
          Seq(true, true, true, true, false, true),
          Seq(true, true, true, true, true, true),
        )
      )
