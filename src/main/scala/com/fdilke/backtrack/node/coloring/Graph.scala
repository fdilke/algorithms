package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.Node
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver
import Graph.adjacencyTableFromPairs
import com.fdilke.backtrack.BacktrackIterable
import com.fdilke.utility.SetsUtilities.*
import com.fdilke.backtrack.node.MonadIterable

import scala.annotation.{tailrec, targetName}
import scala.util.Random

class Graph(
  val adjacencyTable: Seq[Seq[Boolean]],
  val edges: Seq[(Int, Int)]
):
  val vertices: Seq[Int] =
    adjacencyTable.indices
  val numVertices: Int =
    vertices.size

  def sortByDescDegree(): (Seq[Int], Seq[Int]) =
    def degree(vertex: Int): Int =
      adjacencyTable(vertex).count(identity)
    val order: Seq[Int] =
      vertices.sortBy(degree).reverse
    val inverse = invertPermutation(order)
    (order, inverse)

  def checkAntireflexive(): Unit =
    for
      i <- vertices
    do
      if adjacencyTable(i)(i) then
        throw new IllegalArgumentException(s"adjacency table must be antireflexive: fail at $i")

  def checkSymmetric(): Unit =
    for
      i <- vertices
      j <- 0 until i
    do
      if adjacencyTable(j)(i) != adjacencyTable(i)(j) then
        throw new IllegalArgumentException(s"adjacency table must be symmetric: fail at $j, $i")

  def neighborsOf(vertex: Int): Seq[Int] =
    vertices filter
      adjacencyTable(vertex)

  def distanceMap(
    home: Int
  ): Map[Int, Int] =
    @tailrec def distanceMapSub(
      level: Int,
      visited: Set[Int],
      frontier: Set[Int],
      accumulate: Map[Int, Int]
    ): Map[Int, Int] =
      if frontier.isEmpty then
        accumulate
      else
        val newVisited: Set[Int] =
          visited union frontier
        distanceMapSub(
          level = level + 1,
          visited = newVisited,
          frontier =
            frontier.flatMap:
              neighborsOf
            .diff(newVisited),
          accumulate =
            accumulate ++
              frontier.map ( v => v -> level ).toMap
        )
    distanceMapSub(
      level = 0,
      visited = Set.empty,
      frontier = Set(home),
      accumulate = Map(home -> 0)
    )

  def singlePointExtensions(
    images: Seq[Int]
  ): Iterable[Seq[Int]] =
    val nextVertex = images.size
    if nextVertex >= numVertices then
      throw new IllegalArgumentException("map is already complete")
    (0 until numVertices).filterNot: v =>
      images contains v
    .filter: v =>
      images.zipWithIndex.forall:
        (image, i) =>
          adjacencyTable(nextVertex)(i) ==
            adjacencyTable(v)(image)
    .map: v =>
      images :+ v

  def fullExtensions(
    images: Seq[Int]
  ): Iterable[Seq[Int]] =
    class Extension(
      extendedImages: Seq[Int]
    ) extends Node[Extension, Iterable, Seq[Int]]:
      override def explore: Iterable[Either[Extension, Seq[Int]]] =
        if extendedImages.size == numVertices then
          Iterable(solution(extendedImages))
        else
          singlePointExtensions(extendedImages).map:
            furtherExtension => node(Extension(furtherExtension))
    StackSafeNodeSolver.allSolutions[Extension, Iterable, Seq[Int]]:
      Extension(images)

  def singlePointExtensionsMap(
    images: Map[Int, Int]
  ): Iterable[Map[Int, Int]] =
    val unusedVertexes: Seq[Int] =
      vertices.diff(images.keySet.toSeq)
    if images.size >= numVertices then
      throw new IllegalArgumentException("map is already complete")
    val nextVertex: Int =
      unusedVertexes.min
    val unusedImages: Seq[Int] =
      vertices.diff(images.values.toSeq)
    unusedImages.filter: v =>
      images.forall:
        (i, image) =>
          adjacencyTable(nextVertex)(i) ==
            adjacencyTable(v)(image)
    .map: v =>
      images + (nextVertex -> v)

  def fullExtensionsMap(
    imageMap: Map[Int, Int]
  ): Iterable[Map[Int, Int]] =
    BacktrackIterable[Map[Int, Int], Map[Int, Int]](
      imageMap
    ): extension =>
      if extension.size == numVertices then
        Iterable(Right(extension))
      else
        singlePointExtensionsMap(extension).map:
          Left(_)

  lazy val distanceMaps: Seq[Seq[Int]] =
    for
      i <- vertices
    yield
      val map: Map[Int, Int] =
        distanceMap(i)
      vertices.map { map }

  lazy val diameter: Int =
    distanceMaps.flatten.max

  lazy val distanceTransitive: Boolean =
    (for
      i <- vertices
      j <- 0 until i
    yield
      (i, j)
    ).forall: (i, j) =>
      val distance = distanceMaps(i)(j)
      (for
        k <- vertices
        l <- 0 until k if distanceMaps(k)(l) == distance
      yield
        (k, l)
      ).forall: (k, l) =>
        fullExtensionsMap(Map(i -> k, j -> l)).nonEmpty &&
          fullExtensionsMap(Map(k -> i, l -> j)).nonEmpty

  def localIntersectionArray(
    sourceVertex: Int
  ): Option[(Seq[Int], Seq[Int])] =
    val distanceMax = distanceMaps(sourceVertex).max
    val verticesAtDistance: Seq[Seq[Int]] =
      for
        d <- 0 to distanceMax
      yield
        vertices filter: x =>
          distanceMaps(sourceVertex)(x) == d
    def neighboursAt(x: Int, d: Int): Int =
      neighborsOf(x).intersect(verticesAtDistance(d)).size
    allOrNone[Int]:
      for
        j <- 0 until distanceMax
      yield () =>
        crossCheckResult:
          verticesAtDistance(j).map:
            x => () => neighboursAt(x, j + 1)
    match
      case None => None
      case Some(forwards) =>
        allOrNone[Int]:
          for
            j <- 0 until distanceMax
          yield () =>
            crossCheckResult:
              verticesAtDistance(j+1).map:
                x => () => neighboursAt(x, j)
        match
          case None => None
          case Some(backwards) =>
            Some(forwards, backwards)

  lazy val intersectionArray: Option[(Seq[Int], Seq[Int])] =
    crossCheckResultOptional:
      vertices.map: v =>
        () => localIntersectionArray(v)

  lazy val distanceRegular: Boolean =
    intersectionArray.isDefined

object Graph:
  @targetName("applyWithEdges")
  def apply(
    edges: Seq[(Int, Int)]
  ): Graph =
    new Graph(
      adjacencyTableFromPairs(edges *),
      edges
    )

  @targetName("applyWithEdgesVarargs")
  inline def apply(
    edges: (Int, Int)*
  ): Graph =
    Graph:
      edges

  @targetName("applyWithAdjacencyTable")
  def apply(
    adjacencyTable: Seq[Boolean]*
  ): Graph =
    fromAdjacencyTable:
      adjacencyTable

  private def fromAdjacencyTable(
    adjacencyTable: Seq[Seq[Boolean]]
  ): Graph =
    new Graph(
      adjacencyTable,
      edgesFromAdjacencyTable(adjacencyTable)
    )

  @targetName("applyWithUnpackedAdjacencies")
  def apply(
    unpackedAdjacencyTable: Boolean*
  ): Graph =
    fromAdjacencyTable:
      packAdjacencyTable:
        unpackedAdjacencyTable

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
    val lastVertex: Int =
      lastVertexFromPairs(adjacencyPairs*)
    for
      i <- 0 to lastVertex
    yield
      for
        j <- 0 to lastVertex
      yield
        adjacencyPairs.contains(i -> j) ||
          adjacencyPairs.contains(j -> i)

  private def edgesFromAdjacencyTable(
    adjacencyTable: Seq[Seq[Boolean]]
  ): Seq[(Int, Int)] =
    for
      i <- adjacencyTable.indices
      row = adjacencyTable(i)
      j <- 0 until i if row(j)
    yield
      j -> i

  private def packAdjacencyTable(
    unpackedAdjacencyTable: Seq[Boolean]
  ): Seq[Seq[Boolean]] =
    squareUp(unpackedAdjacencyTable*)

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
      if afterRegion < circleSize then
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
  ): Graph =
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
    Graph(adjacencies._2)

  def torus(
    width: Int,
    height: Int
  ): Graph =
    if width == 0 || height == 0 then
      Graph(Seq.empty[Boolean]*)
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
      Graph:
        pairs.flatten.distinct

//"The odd graph O_{n} has one vertex for each of the (n-1)-element subsets of a (2n-1)}-element set.
//Two vertices are connected by an edge if and only if the corresponding subsets are disjoint."

  def oddGraph(n: Int): Graph =
    val tuples: Seq[Seq[Int]] =
      (0 until (2*n - 1)).combinations(n - 1).toSeq
    Graph:
      for
        i <- tuples.indices
        tupleI = tuples(i)
        j <- 0 until i if tupleI.intersect(tuples(j)).isEmpty
      yield
        (j, i)

  def completeGraph(
    vertices: Int
  ): Graph =
    Graph:
      for
        i <- 0 until vertices
        j <- 0 until i
      yield
        i -> j

  def completeBipartite(
    from: Int,
    to: Int
  ): Graph =
    Graph:
      for
        i <- 0 until from
        j <- 0 until to
      yield
        i -> (from + j)

  lazy val emptyGraph: Graph =
    Graph(Seq.empty[Seq[Boolean]]*)

  lazy val onePointGraph: Graph =
    Graph(false)

  lazy val petersen: Graph =
    oddGraph(3)

  lazy val heawood: Graph =
    hamiltonianCubic(14, 5, -5)

  private def hamiltonianCubic(vertices: Int, cycle: Int*): Graph =
    val circumference: Seq[(Int, Int)] =
      0 until vertices map: v =>
        (v, (v + 1) % vertices)
    val crosslinks: Seq[(Int, Int)] =
      0 until vertices map: v =>
        val direction = cycle(v % cycle.length)
        (v, (v + vertices + direction) % vertices)
    Graph:
      circumference ++ crosslinks

  lazy val pappus: Graph =
    hamiltonianCubic(18, -5, 5, 7, -7, 7, -7)

  lazy val cubicalGraph: Graph =
    hamiltonianCubic(8, 3, -3)

  lazy val dodecahedralGraph: Graph =
    hamiltonianCubic(20, 4, -4, -7, 10, -4, 7, -7, 4, 10, 7, 4, -4, -7, 10, -4, 7, -7, 4, -10, 7)

  lazy val tetrahedralGraph: Graph =
    completeGraph(4)

  lazy val shrikhande: Graph =
    val size = 4
    def vertexAt(x: Int, y: Int): Int =
      (x % size) + (y % size) * 4
    Graph:
      (for
        x <- 0 until 4
        y <- 0 until 4
        vertex = vertexAt(x, y)
      yield
        Seq(
          vertex -> vertexAt(x + 1, y),
          vertex -> vertexAt(x, y + 1),
          vertex -> vertexAt(x + 1, y + 1)
        )
      ).flatten
