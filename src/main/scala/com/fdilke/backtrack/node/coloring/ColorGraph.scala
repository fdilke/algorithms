package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.NodeIterable
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver
import com.fdilke.backtrack.node.MonadIterable.*
import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.squareUp

import scala.annotation.targetName

object ColorGraph:
  @targetName("applyWithAdjacencyPairs")
  def apply(
    numColors: Int,
    adjacencyPairs: (Int, Int)*
  ): Option[Seq[Int]] =
    apply(numColors, adjacencyTableFromPairs(adjacencyPairs*))

  def apply(
    numColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Option[Seq[Int]] =
    checkAntireflexive(adjacencyTable)
    checkSymmetric(adjacencyTable)
    class PartialColoring(
      colors: Seq[Int]
    ) extends NodeIterable[Seq[Int]]:
      override def explore: NodeStatus =
        Iterable.empty[NodeChoice]
    Some(adjacencyTable.indices)

  @targetName("applyWithUnpackedAdjacencyTable")
  def apply(
     numColors: Int,
     unpackedAdjacencyTable: Boolean*
   ): Option[Seq[Int]] =
    apply(numColors, packAdjacencyTable(unpackedAdjacencyTable))

  private def checkAntireflexive(
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    for
      i <- adjacencyTable.indices
    do
      if adjacencyTable(i)(i) then
        throw new IllegalArgumentException(s"adjacency table must be antireflexive: fail at $i")

  private def checkSymmetric(
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    for
      i <- adjacencyTable.indices
      j <- 0 until i
    do
      if adjacencyTable(j)(i) != adjacencyTable(i)(j) then
        throw new IllegalArgumentException(s"adjacency table must be symmetric: fail at $j, $i")


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
    val lastVertex: Int =
      if adjacencyPairs.isEmpty then
        -1
      else
        adjacencyPairs.flatMap: (v, w) =>
          Seq(v, w)
        .max
    for
      i <- 0 to lastVertex
    yield
      for
        j <- 0 to lastVertex
      yield
        adjacencyPairs.contains(i -> j) ||
          adjacencyPairs.contains(j -> i)

  def packAdjacencyTable(
    unpackedAdjacencyTable: Seq[Boolean]
  ): Seq[Seq[Boolean]] =
    squareUp(unpackedAdjacencyTable*)

        