package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.MonadIterable.*
import com.fdilke.backtrack.node.NodeIterable
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver

class PartialColoring(
  numVertices: Int,
  adjacencies: Seq[Seq[Boolean]],
  colors: Seq[Int],
  colorAdjacencies: Seq[Seq[Boolean]]
):
  val distinctColors: Seq[Int] = colors.distinct
  def amalgamations: Seq[(Int, Int)] =
    for
      c <- distinctColors
      d <- distinctColors if (c < d && !colorAdjacencies(c)(d))
    yield
      (c, d)
    

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


