package com.fdilke.backtrack.node.coloring

class DescendingDegree(
  algo: ColoringAlgo
) extends ColoringAlgo:
  def apply(
    targetNumColors: Int,
    graph: Graph
  ): Option[Seq[Int]] =
    val indices: Seq[Int] =
      graph.adjacencyTable.indices
    val (order, inverse): (Seq[Int], Seq[Int]) =
      graph.sortByDescDegree()
    val remappedAdjs: Seq[Seq[Boolean]] =
      for
        i <- indices
        oi = order(i)
        atoi = graph.adjacencyTable(oi)
      yield
        for
          j <- indices
          oj = order(j)
        yield
          atoi(oj)
    algo.apply(
      targetNumColors,
      Graph(remappedAdjs*)
    ) map: remappedColors =>
      indices.map:
        i => remappedColors(inverse(i))

