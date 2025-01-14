package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.NodeIterable
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver
import com.fdilke.backtrack.node.MonadIterable._

object ColorGraph:
  def apply(
    numColors: Int,
    adjacencies: Seq[Seq[Boolean]]
  ): Option[Seq[Int]] =
    class PartialColoring(
      colors: Seq[Int]
    ) extends NodeIterable[Seq[Int]]:
      override def explore: NodeStatus =
        Iterable.empty[NodeChoice]
    Some(0 until adjacencies.length)

//        val distinctColors: Seq[Int] = colors.distinct
//        val possibleIdentifications =
//          for
//            c <- distinctColors
//            d <- distinctColors if c != d
//              ...
//

//    StackSafeNodeSolver.allSolutions:
//      PartialColoring(0 until numVertexes)
//    .head


