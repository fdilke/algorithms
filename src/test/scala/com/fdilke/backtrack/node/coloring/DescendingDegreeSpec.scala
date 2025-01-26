package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

class DescendingDegreeSpec  extends FunSuite:
  test("applies an algo via the degree sorting"):
    val graphAdjacencies: Seq[Seq[Boolean]] =
      Seq(
        Seq(false, true, true, false, false),
        Seq(true, false, false, true, true),
        Seq(true, false, false, false, true),
        Seq(false, true, false, false, true),
        Seq(false, true, true, true, false)
      )
    val saveColors: AtomicInteger =
      new AtomicInteger(-1)
    val saveAdjacencyTable: AtomicReference[Seq[Seq[Boolean]]] =
      new AtomicReference[Seq[Seq[Boolean]]](Seq.empty)
    val algo: GraphColoringAlgo =
      (targetNumColors: Int, adjacencyTable: Seq[Seq[Boolean]]) =>
        saveColors.set(targetNumColors)
        saveAdjacencyTable.set(adjacencyTable)
        Some(Seq(0, 1, 2, 3, 4))
    DescendingDegree(algo).apply(
      3,
      graphAdjacencies
    ) match
      case None => fail("coloring algo failed")
      case Some(inverse) =>
        saveColors.get is 3
        inverse.toSet is graphAdjacencies.indices.toSet
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
          adjTab(i)(j) is graphAdjacencies(order(i))(order(j))

//  test("additional sanity test"):
//    val graphAdjacencies: Seq[Seq[Boolean]] =
//      GraphConstructions.adjacencyTableFromPairs(1 -> 1)
//    val algo: GraphColoringAlgo =
//      (targetNumColors: Int, adjacencyTable: Seq[Seq[Boolean]]) =>
//        println("remappedAdj:")
//        println(adjacencyTable)
//        adjacencyTable is Seq.empty
//        None
//    DescendingDegree(algo)(2, graphAdjacencies)
