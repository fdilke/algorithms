package com.fdilke.apps

import com.fdilke.algebra.permutation.{Group, GroupSugar, Permutation}
import GroupSugar.*
import com.fdilke.backtrack.node.coloring.{ColorGraphLoop, Graph, PartialColoring}

// Attempt to find permutations f, v, e of order 4, 5, 2 respectively with fv = e
// These then form the generators of a finite homomorphic image of the von Dyck group D(4, 5, 2)

object HedronQuest extends App:
  private val searchDegree = 5
  val group = Permutation.group(searchDegree)
  given Group[Permutation] = group
  private def elementsOfOrder(n: Int) =
    group.elements.filter:
      _.order == n
  private def conjugacyRepresentatives(elements: Set[Permutation]): Set[Permutation] =
    elements.map:
      _.canonicalConjugate
  val order4s = elementsOfOrder(4)
  val order4sReduced = conjugacyRepresentatives(order4s)
  val order5s = elementsOfOrder(5)
  val order5sReduced = conjugacyRepresentatives(order5s)
  println(s"${order4s.size} -> ${order4sReduced.size} elements of order 4")
  println(s"${order5s.size} -> ${order5sReduced.size} elements of order 5")
  val (loop4: Set[Permutation], loop5: Set[Permutation]) =
    if (order4sReduced.size * order5s.size) < (order4s.size * order5sReduced.size) then
      println("Winnowing 4")
      (order4sReduced, order5s)
    else
      println("Winnowing 5")
      (order4s, order5sReduced)

  case class Hedron(
    f: Permutation,
    v: Permutation,
    subgroup: group.Subgroup
  ):
    val order: Int =
      subgroup.order
  private val hedrons: Set[Hedron] =
    for
      f <- loop4
      v <- loop5
      e = f * v if e.order == 2
    yield
      val subgroup = group.generateSubgroup(f, v)
      Hedron(f, v, subgroup)
  private val orders: Seq[Int] =
    hedrons.map { _.order }.toSeq.sorted
  println("Sorted orders: " + orders.mkString(","))
  val smallestOrder = orders.head
  val plushie: Hedron =
    hedrons.find:
      _.order == smallestOrder
    .getOrElse:
      throw new IllegalArgumentException("can't find smallest plushie")
  println("for the small plushie:")
  println("f = " + plushie.f)
  println("v = " + plushie.v)

object PlushiePlayground extends App:
  val group: Group[Permutation] = Permutation.group(5)
  given Group[Permutation] = group
  val f = Permutation(0, 2, 3, 4, 1)
  val v = Permutation(2, 4, 1, 0, 3)
  val e = f * v
  assert(e.order == 2)
  val plushieGroup: group.Subgroup =
    group.generateSubgroup(f, v)
  println(s"Plushie group order = ${plushieGroup.order}")
  type Coset = Set[Permutation]
  type Corner= (Coset, Coset)
  def multiplyCoset(
     coset: Coset,
     g: Permutation
   ): Coset =
    coset.map:
      s => s * g
  def rightCosetsOf(subgroup: group.Subgroup): Seq[Coset] =
    group.elements.map: g =>
      multiplyCoset(subgroup.elements, g)
    .toSeq
  def incident(c1: Coset, c2: Coset): Boolean =
    (c1 intersect c2).nonEmpty
  val fGroup: group.Subgroup = group.generateSubgroup(f)
  val vGroup: group.Subgroup = group.generateSubgroup(v)
  val eGroup: group.Subgroup = group.generateSubgroup(e)
  val vertexes = rightCosetsOf(fGroup)
  val faces = rightCosetsOf(vGroup)
  val edges = rightCosetsOf(eGroup)
  println(s"${vertexes.size} vertexes")
  println(s"${faces.size} faces")
  println(s"${edges.size} edges")
  val corners: Seq[Corner] =
    for
      face <- faces
      vertex <- vertexes if incident(face, vertex)
    yield
      (face, vertex)
  println(s"${corners.size} corners")
  def multiplyCorner(
    corner: Corner,
    g: Permutation
  ): Corner =
    (
      multiplyCoset(corner._1, g),
      multiplyCoset(corner._2, g)
    )
  val sampleCorner: Corner = corners.head
  val cornerCoset: Set[Corner] =
    group.elements.map: g =>
      multiplyCorner(sampleCorner, g)
  if cornerCoset.size == group.order then
    println("Group acts torsorially on corners")
  else
    println("Not torsorial :(")
  def checkIncidences(
    seq1: Seq[Coset],
    seq2: Seq[Coset],
    theCount: Int
  ): Boolean =
    seq1.forall: s1 =>
      seq2.count: s2 =>
        incident(s1, s2)
      == theCount
  if checkIncidences(faces, vertexes, 5) then
    println("Every face is incident to 5 vertexes")
  if checkIncidences(faces, edges, 5) then
    println("Every face is incident to 5 edges")
  if checkIncidences(vertexes, faces, 4) then
    println("Every vertex is incident to 4 faces")
  if checkIncidences(vertexes, edges, 4) then
    println("Every vertex is incident to 4 edges")
  if checkIncidences(edges, vertexes, 2) then
    println("Every edge is incident to 2 vertexes")
  if checkIncidences(vertexes, edges, 4) then
    println("Every vertex is incident to 4 edges")

  def adjacentFaces(f1: Coset, f2: Coset): Boolean =
    edges.exists: edge =>
      incident(edge, f1) && incident(edge, f2)

  val faceAdjacencies: Seq[(Int, Int)] =
    for
      (faceI, i) <- faces.zipWithIndex
      (faceJ, j) <- faces.take(i).zipWithIndex if adjacentFaces(faceI, faceJ)
    yield
      (i, j)
  println("# face adjacencies: " + faceAdjacencies.size)
  def label(i: Int): Char =
    (65 + i).toChar

  for
    i <- faces.indices
  do
    print(s"${label(i)}: ")
    for
      j <- faces.indices if faceAdjacencies.contains(i, j) || faceAdjacencies.contains(j, i)
    do
      print(s"${label(j)} ")
    println("")
  val faceGraph = Graph(faceAdjacencies*)
  for (n <- 1 to 6) do
    if ColorGraphLoop(n, faceGraph).isDefined then
      println(s"Plushie can be $n-colored")
    else
      println(s"Plushie cannot be $n-colored")
  private def checkMinColoring(
     numColors: Int,
     coloring: Seq[Int],
     graph: Graph,
   ): Unit =
      for
        i <- coloring.indices
        j <- 0 until i
      do
        if graph.adjacencyTable(i)(j) && (coloring(i) == coloring(j)) then
          throw new IllegalArgumentException("adjacent vertices have same color")
      if coloring.distinct.size > numColors then
        throw new IllegalArgumentException("too many colors")
      if PartialColoring.fromColorsAndGraph(
        coloring,
        graph
      ).amalgamations.nonEmpty then
        throw new IllegalArgumentException("not a minimal coloring: an amalgamation is possible")

  ColorGraphLoop(2, faceGraph) match
    case None => throw new IllegalArgumentException("no coloring after all")
    case Some(coloring) =>
      checkMinColoring(2, coloring, faceGraph)
      println("the 2-coloring: " + coloring.map{ label }.mkString(""))

  def adjacentVertexes(v1: Coset, v2: Coset): Boolean =
    edges.exists: edge =>
      incident(edge, v1) && incident(edge, v2)

  val vertexAdjacencies: Seq[(Int, Int)] =
    for
      (vertexI, i) <- vertexes.zipWithIndex
      (vertexJ, j) <- vertexes.take(i).zipWithIndex if adjacentVertexes(vertexI, vertexJ)
    yield
      (i, j)
  println("# vertex adjacencies: " + vertexAdjacencies.size)
  val vertexGraph = Graph(vertexAdjacencies*)
  ColorGraphLoop(3, vertexGraph) match
    case None => throw new IllegalArgumentException("no coloring")
    case Some(coloring) =>
      checkMinColoring(3, coloring, vertexGraph)
      println("the vertex coloring: " + coloring.map { label }.mkString(""))

  def adjacentEdges(e1: Coset, e2: Coset): Boolean =
    vertexes.exists: vertex =>
      incident(vertex, e1) && incident(vertex, e2)
  if false then // a bit of a slow calculation
    val edgeAdjacencies: Seq[(Int, Int)] =
      for
        (edgeI, i) <- edges.zipWithIndex
        (edgeJ, j) <- edges.take(i).zipWithIndex if adjacentEdges(edgeI, edgeJ)
      yield
        (i, j)
    println("# edge adjacencies: " + edgeAdjacencies.size)
    val edgeGraph = Graph(edgeAdjacencies*)
    ColorGraphLoop(4, edgeGraph) match
      case None => throw new IllegalArgumentException("no coloring")
      case Some(coloring) =>
        checkMinColoring(4, coloring, edgeGraph)
        println("the edge coloring: " + coloring.map { label }.mkString(""))
  val distanceMaps: Seq[Seq[Int]] =
    for
      i <- faces.indices
    yield
      val map: Map[Int, Int] =
        faceGraph.distanceMap(i)
      faces.indices.map { map }
  for
    i <- faces.indices
    j <- 0 until i
  do
    val distance = distanceMaps(i)(j)
    for
      k <- faces.indices
      l <- 0 until k if distanceMaps(k)(l) == distance
    do
      if !plushieGroup.elements.exists: g =>
        multiplyCoset(faces(i), g) == faces(k) &&
          multiplyCoset(faces(j), g) == faces(l)
      then
        throw IllegalArgumentException(s"can't map face pair: $i, $j -> $k, $l at distance $distance")
