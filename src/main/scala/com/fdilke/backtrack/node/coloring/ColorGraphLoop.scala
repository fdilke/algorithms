package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.{MonadIterable, Node}
import com.fdilke.backtrack.node.NodeSolvers.StackSafeDedupNodeSolver
import com.fdilke.backtrack.node.coloring.GraphConstructions.*

object ColorGraphLoop extends GraphColoringAlgo:
  override def apply(
    targetNumColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Option[Seq[Int]] =
    val numVertices: Int = adjacencyTable.size
    checkAntireflexive(adjacencyTable)
    checkSymmetric(adjacencyTable)
    class PartialVertexColoring(
      coloring: Seq[Int]
    ) extends Node[PartialVertexColoring, Iterable, Seq[Int]]:
      def safeToColor(vertex: Int, color: Int): Boolean =
        !coloring.indices.filter:
          adjacencyTable(vertex)(_)
        .exists: neighbor =>
          coloring(neighbor) == color
      override def explore: Iterable[Either[PartialVertexColoring, Seq[Int]]] =
        val nextVertex = coloring.size
        if nextVertex == numVertices then
          Iterable(solution(coloring))
        else
          for
            color <- 0 until targetNumColors if safeToColor(nextVertex, color)
          yield
            node(PartialVertexColoring(coloring :+ color))
    StackSafeDedupNodeSolver.allSolutions[PartialVertexColoring, Iterable, Seq[Int]]:
      PartialVertexColoring(
        Seq.empty
      )
    .headOption



        