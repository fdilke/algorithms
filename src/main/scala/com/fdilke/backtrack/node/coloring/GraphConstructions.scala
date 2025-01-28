package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.squareUp

object GraphConstructions:
  def torus(
    width: Int,
    height: Int
  ): Seq[(Int, Int)] =
    if width == 0 || height == 0 then
      Seq.empty
    else
      def cellIndex(i: Int, j: Int): Int =
        (i % width) * height + (j % height)
      val pairs =
        for
          i <- 0 to width
          j <- 0 to height
        yield
          Seq(
            cellIndex(i, j) -> cellIndex(i + 1, j),
            cellIndex(i, j) -> cellIndex(i, j + 1)
          )
      pairs.flatten.distinct

//"The odd graph O_{n} has one vertex for each of the (n-1)-element subsets of a (2n-1)}-element set.
//Two vertices are connected by an edge if and only if the corresponding subsets are disjoint."

  def oddGraph(n: Int): Seq[(Int, Int)] =
    val tuples: Seq[Seq[Int]] =
      (0 until (2*n - 1)).combinations(n - 1).toSeq
    for
      i <- tuples.indices
      tupleI = tuples(i)
      j <- 0 until i if tupleI.intersect(tuples(j)).isEmpty
    yield
      (j, i)

  def checkAntireflexive(
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    for
      i <- adjacencyTable.indices
    do
      if adjacencyTable(i)(i) then
        throw new IllegalArgumentException(s"adjacency table must be antireflexive: fail at $i")

  def checkSymmetric(
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    for
      i <- adjacencyTable.indices
      j <- 0 until i
    do
      if adjacencyTable(j)(i) != adjacencyTable(i)(j) then
        throw new IllegalArgumentException(s"adjacency table must be symmetric: fail at $j, $i")

  def lastVertexFromPairs(
     adjacencyPairs: (Int, Int)*
   ): Int =
    if adjacencyPairs.isEmpty then
      -1
    else
      adjacencyPairs.flatMap: (v, w) =>
        Seq(v, w)
      .max

  def adjacencyTableFromPairs(
    adjacencyPairs: (Int, Int)*
  ): Seq[Seq[Boolean]] =
    val lastVertex: Int = lastVertexFromPairs(adjacencyPairs*)
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

  def sortByDescDegree(
    adjacencyTable: Seq[Seq[Boolean]]
  ): (Seq[Int], Seq[Int]) =
    val indices: Seq[Int] = adjacencyTable.indices
    def degree(vertex: Int): Int =
      adjacencyTable(vertex).count(identity)
    val order: Seq[Int] =
      indices.sortBy(degree).reverse
    val inverse = SetsUtilities.invertPermutation(order)
    (order, inverse)
