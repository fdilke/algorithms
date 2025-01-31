package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.squareUp

import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

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

  // Add a new "layer" (1 cell) to a circle of cells, at the specified index and length.
  // We return the new "outer layer" and a list of any new edges.
  def addLayer(
    circle: Seq[Int],
    newVertex: Int,
    startIndex: Int,
    coverLength: Int
  ): (Seq[Int], Seq[(Int, Int)]) =
    val circleSize = circle.size
    if startIndex < 0 then
      throw new IllegalArgumentException("startIndex < 0")
    else if startIndex >= circleSize then
      throw new IllegalArgumentException("startIndex >= circle size")
    else if coverLength <= 0 then
      throw new IllegalArgumentException("coverLength <= 0")
    else if coverLength > circleSize then
      throw new IllegalArgumentException("coverLength > circle size")
    else if startIndex >= circleSize then
      throw new IllegalArgumentException("startIndex >= circle size")
    else
      val afterRegion: Int =
        startIndex + coverLength
      if (afterRegion < circleSize) then
        (
          (circle.take(startIndex + 1) :+ newVertex) ++ circle.slice(afterRegion - 1, circleSize),
          (startIndex until afterRegion) map: i =>
            circle(i) -> newVertex
        )
      else
        val wrap: Int =
          afterRegion - circleSize
        val decAfterRegion: Int =
          (afterRegion + circleSize - 1) % circleSize
        (
          newVertex +: circle(decAfterRegion) +: circle.slice(wrap, startIndex + 1),
          (startIndex until circleSize) ++ (0 until wrap) map : i =>
            circle (i) -> newVertex
        )

  def randomPlanar(
    numVertices: Int,
    random: Random
  ): Seq[Seq[Boolean]] =
    val adjacencies: (Seq[Int], Seq[(Int, Int)]) =
      (1 until numVertices).foldLeft[(Seq[Int], Seq[(Int, Int)])]((Seq(0), Seq.empty)):
        case ((cycle, edges), v) =>
          val start = random.nextInt(cycle.size)
          val coverLength =
            if cycle.size > 1 then
              1 + random.nextInt(cycle.size - 1)
            else
              1
          val (newCycle, moreEdges) =
            addLayer(cycle, v, start, coverLength)
          (newCycle, edges ++ moreEdges)
    adjacencyTableFromPairs(adjacencies._2*)
