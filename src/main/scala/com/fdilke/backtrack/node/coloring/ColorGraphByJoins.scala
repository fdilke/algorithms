package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.Node
import com.fdilke.backtrack.node.NodeSolvers.StackSafeDedupNodeSolver
import com.fdilke.backtrack.node.MonadIterable

object ColorGraphByJoins extends ColoringAlgo:
  override def apply(
    targetNumColors: Int,
    graph: Graph
  ): Option[Seq[Int]] =
    val numVertices: Int = graph.numVertices
    graph.checkAntireflexive()
    graph.checkSymmetric()
    StackSafeDedupNodeSolver.allSolutions[PartialColoring, Iterable, Seq[Int]]:
      PartialColoring(
        0 until numVertices,
        graph.adjacencyTable
      )
    .find: colors =>
      colors.distinct.size <= targetNumColors



        