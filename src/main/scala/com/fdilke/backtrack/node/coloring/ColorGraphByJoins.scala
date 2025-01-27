package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.Node
import com.fdilke.backtrack.node.NodeSolvers.StackSafeDedupNodeSolver
import com.fdilke.backtrack.node.MonadIterable
import com.fdilke.backtrack.node.coloring.GraphConstructions._

object ColorGraphByJoins extends ColoringAlgo:
  override def apply(
    targetNumColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Option[Seq[Int]] =
    val numVertices: Int = adjacencyTable.size
    checkAntireflexive(adjacencyTable)
    checkSymmetric(adjacencyTable)
    StackSafeDedupNodeSolver.allSolutions[PartialColoring, Iterable, Seq[Int]]:
      PartialColoring(
        0 until numVertices,
        adjacencyTable
      )
    .find: colors =>
      colors.distinct.size <= targetNumColors



        