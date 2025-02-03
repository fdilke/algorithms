package com.fdilke.backtrack.node.coloring

import scala.annotation.targetName

trait ColoringAlgo:
  def apply(
    targetNumColors: Int,
    graph: Graph
  ): Option[Seq[Int]]

object NoEffortColoring extends ColoringAlgo:
  override def apply(
     targetNumColors: Int,
     graph: Graph
   ): Option[Seq[Int]] =
    Some(graph.adjacencyTable.indices)
  