package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver
import com.fdilke.backtrack.node.{MonadIterable, Node}

object ColorGraphLoop extends ColoringAlgo:
  override def apply(
    targetNumColors: Int,
    graph: Graph
  ): Option[Seq[Int]] =
    graph.checkAntireflexive()
    graph.checkSymmetric()
    class PartialVertexColoring(
      coloring: Seq[Int]
    ) extends Node[PartialVertexColoring, Iterable, Seq[Int]]:
      def safeToColor(vertex: Int, color: Int): Boolean =
        !coloring.indices.filter:
          graph.adjacencyTable(vertex)(_)
        .exists: neighbor =>
          coloring(neighbor) == color
      override def explore: Iterable[Either[PartialVertexColoring, Seq[Int]]] =
        val nextVertex = coloring.size
        if nextVertex == graph.numVertices then
          Iterable(solution(coloring))
        else
          for
            color <- 0 until targetNumColors if safeToColor(nextVertex, color)
          yield
            node(PartialVertexColoring(coloring :+ color))
    StackSafeNodeSolver.allSolutions[PartialVertexColoring, Iterable, Seq[Int]]:
      PartialVertexColoring(
        Seq.empty
      )
    .headOption

object ColorGraphTweakedLoop extends DescendingDegree(ColorGraphLoop)

        