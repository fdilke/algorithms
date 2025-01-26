package com.fdilke.backtrack.node.coloring

import scala.annotation.targetName
import com.fdilke.backtrack.node.coloring.GraphConstructions._

trait GraphColoringAlgo:
  def apply(
    targetNumColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Option[Seq[Int]]

  @targetName("applyWithAdjacencyPairs")
  def apply(
    numColors: Int,
    adjacencyPairs: (Int, Int)*
  ): Option[Seq[Int]] =
    apply(numColors, adjacencyTableFromPairs(adjacencyPairs*))

  @targetName("applyWithUnpackedAdjacencyTable")
  def apply(
     numColors: Int,
     unpackedAdjacencyTable: Boolean*
   ): Option[Seq[Int]] =
    apply(numColors, packAdjacencyTable(unpackedAdjacencyTable))

