package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.NodeIterable
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver
import com.fdilke.backtrack.node.MonadIterable.*

import scala.annotation.targetName

object ColorGraph:
  @targetName("applyWithAdjecencyPairs")
  def apply(
    numColors: Int,
    adjacencyPairs: (Int, Int)*
  ): Option[Seq[Int]] =
    apply(numColors, adjacencyTableFromPairs(adjacencyPairs*))
    
  def apply(
    numColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Option[Seq[Int]] =
    class PartialColoring(
      colors: Seq[Int]
    ) extends NodeIterable[Seq[Int]]:
      override def explore: NodeStatus =
        Iterable.empty[NodeChoice]
    Some(adjacencyTable.indices)

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

  def adjacencyTableFromPairs(
    adjacencyPairs: (Int, Int)*
  ): Seq[Seq[Boolean]] =
//    val highest: Int =
    Seq()      
