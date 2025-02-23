package com.fdilke.apps

import com.fdilke.algebra.permutation.{Group, GroupSugar, Permutation}
import GroupSugar.*
import com.fdilke.backtrack.node.coloring.{ColorGraphLoop, Graph, PartialColoring}

// Attempt to find permutations f, v, e of order 4, 5, 2 respectively with fv = e
// These then form the generators of a finite homomorphic image of the von Dyck group D(4, 5, 2)

object HedronQuest extends App:
  private val searchDegree = 5
  val group = Permutation.symmetricGroup(searchDegree)
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

object PretzelPlushie extends PlushiePlayground(
  f = Permutation(0, 2, 3, 4, 1),
  v = Permutation(2, 4, 1, 0, 3)
)

class PlushiePlayground(
  f: Permutation,
  v: Permutation
) extends App:
  val group: Group[Permutation] = Permutation.symmetricGroup(
    Math.max(f.degree, v.degree)
  )
  given Group[Permutation] = group
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
  val chi: Int = vertexes.size - edges.size + faces.size
  println(s"χ = $chi")
  val genus: Int = 1 - chi/2
  println(s"genus = $genus")
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
    subg1: group.Subgroup,
    subg2: group.Subgroup,
  ): String =
    val theCount: Int =
      subg1.order /
        (subg1.elements.intersect(subg2.elements).size)
    if
      rightCosetsOf(subg1).forall: s1 =>
        rightCosetsOf(subg2).count: s2 =>
          incident(s1, s2)
        == theCount
    then
      theCount.toString
    else
      "?"

  println(s"Every face is incident to ${checkIncidences(vGroup, fGroup)} vertexes")
  println(s"Every face is incident to ${checkIncidences(vGroup, eGroup)} edges")
  println(s"Every vertex is incident to ${checkIncidences(fGroup, vGroup)} faces")
  println(s"Every vertex is incident to ${checkIncidences(fGroup, eGroup)} edges")
  println(s"Every edge is incident to ${checkIncidences(eGroup, fGroup)} vertexes")
  println(s"Every vertex is incident to ${checkIncidences(fGroup, eGroup)} edges")

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
  val faceGraph = Graph(faceAdjacencies)
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
  val vertexGraph = Graph(vertexAdjacencies)
  ColorGraphLoop(3, vertexGraph) match
    case None => throw new IllegalArgumentException("no coloring")
    case Some(coloring) =>
      checkMinColoring(3, coloring, vertexGraph)
      println("the vertex coloring: " + coloring.map { label }.mkString(""))

  def adjacentEdges(e1: Coset, e2: Coset): Boolean =
    vertexes.exists: vertex =>
      incident(vertex, e1) && incident(vertex, e2)
  val edgeAdjacencies: Seq[(Int, Int)] =
    for
      (edgeI, i) <- edges.zipWithIndex
      (edgeJ, j) <- edges.take(i).zipWithIndex if adjacentEdges(edgeI, edgeJ)
    yield
      (i, j)
  println("# edge adjacencies: " + edgeAdjacencies.size)
  val edgeGraph = Graph(edgeAdjacencies)
  if false then // a bit of a slow calculation
    ColorGraphLoop(4, edgeGraph) match
      case None => throw new IllegalArgumentException("no coloring")
      case Some(coloring) =>
        checkMinColoring(4, coloring, edgeGraph)
        println("the edge coloring: " + coloring.map { label }.mkString(""))
  println("faceGraph is distance-transitive:" + faceGraph.distanceTransitive)
  if false then
    println("vertexGraph is distance-transitive:" + vertexGraph.distanceTransitive)
    println("edgeGraph is distance-transitive:" + edgeGraph.distanceTransitive)
  println("faceGraph is distance-regular:" + faceGraph.distanceRegular)
  println("vertexGraph is distance-regular:" + vertexGraph.distanceRegular)
//  if false then
  println("edgeGraph is distance-regular:" + edgeGraph.distanceRegular)

  println("faceGraph is Cayley:" + faceGraph.cayley)
  println("vertexGraph is Cayley:" + vertexGraph.cayley)
  println("edgeGraph is Cayley:" + edgeGraph.cayley)
