package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.MonadIterable.*
import com.fdilke.backtrack.node.NodeIterable
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver

case class PartialColoring(
  numVertices: Int,
  adjacencies: Seq[Seq[Boolean]],
  colors: Seq[Int],
  colorAdjacencies: Seq[Seq[Boolean]]
):
  val distinctColors: Seq[Int] = colors.distinct
//  val numColors: Int = distinctColors.size
  def amalgamations: Seq[(Int, Int)] =
    for
      c <- distinctColors
      d <- distinctColors if c < d && !colorAdjacencies(c)(d)
    yield
      (c, d)

  def amalgamate(
    colorPair: (Int, Int)
  ): PartialColoring =
    val (c, d) = colorPair
    val newColors: Seq[Int] =
      colors.map:
        color =>
        if (color == d) c else color
    val newColorAdjacencies =
      for
        e <- 0 until numVertices
      yield
        for
          f <- 0 until numVertices
        yield
          colorAdjacencies(e)(f) ||
          (e == c && colorAdjacencies(d)(f)) ||
          (f == c && colorAdjacencies(e)(d)) ||
          e == d || f == d
    println("New c adjs:")
    for (xx <- newColorAdjacencies)
      println(">> " + xx)
    PartialColoring(
      numVertices,
      adjacencies,
      newColors,
      newColorAdjacencies
    )

// extends NodeIterable[Seq[Int]]:
//      override def explore: NodeStatus =
//        Iterable.empty[NodeChoice]

//        val distinctColors: Seq[Int] = colors.distinct
//        val possibleIdentifications =
//          for
//            c <- distinctColors
//            d <- distinctColors if c != d
//              ...
//


