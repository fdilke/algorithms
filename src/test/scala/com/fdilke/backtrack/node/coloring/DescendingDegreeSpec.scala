package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

class DescendingDegreeSpec extends FunSuite:
  test("applies an algo via the degree sorting"):
    val graph: Graph =
      Graph(
        false, true, true, false, false,
        true, false, false, true, true,
        true, false, false, false, true,
        false, true, false, false, true,
        false, true, true, true, false
      )
    val saveColors: AtomicInteger =
      new AtomicInteger(-1)
    val saveAdjacencyTable: AtomicReference[Seq[Seq[Boolean]]] =
      new AtomicReference[Seq[Seq[Boolean]]](Seq.empty)
    val algo: ColoringAlgo =
      (targetNumColors: Int, graph: Graph) =>
        saveColors.set(targetNumColors)
        saveAdjacencyTable.set(graph.adjacencyTable)
        Some(Seq(0, 1, 2, 3, 4))
    DescendingDegree(algo).apply(
      3,
      graph
    ) match
      case None => fail("coloring algo failed")
      case Some(inverse) =>
        saveColors.get is 3
        inverse.toSet is (0 until graph.numVertices).toSet
        val order: Array[Int] = Array[Int](inverse.indices*)
        for
          i <- inverse.indices
        do
          order(inverse(i)) = i
        val adjTab: Seq[Seq[Boolean]] = saveAdjacencyTable.get()
        for
          i <- inverse.indices
          j <- inverse.indices
        do
          adjTab(i)(j) is graph.adjacencyTable(order(i))(order(j))
