package com.fdilke.backtrack.node.coloring

class DescendingDegree(
  algo: GraphColoringAlgo
) extends GraphColoringAlgo:
  def apply(
    targetNumColors: Int,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Option[Seq[Int]] =
    val indices: Seq[Int] =
      adjacencyTable.indices
    val (order, inverse): (Seq[Int], Seq[Int]) =
      GraphConstructions.sortByDescDegree(adjacencyTable)
    val remappedAdjs: Seq[Seq[Boolean]] =
      for
        i <- indices
        oi = order(i)
        atoi = adjacencyTable(oi)
      yield
        for
          j <- indices
          oj = order(j)
        yield
          atoi(oj)
    algo.apply(
      targetNumColors,
      remappedAdjs
    ) map: remappedColors =>
      indices.map:
        i => remappedColors(inverse(i))

