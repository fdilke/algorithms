package com.fdilke.apps

import com.fdilke.algebra.permutation.{Group, GroupSugar, Permutation}
import GroupSugar.*
import com.fdilke.backtrack.node.coloring.{ColorGraphLoop, Graph, PartialColoring}

// Attempt to find permutations f, v, e of order 4, 5, 2 respectively with fv = e
// These then form the generators of a finite homomorphic image of the von Dyck group D(4, 5, 2)

object PentagridPlushieQuest extends HedronQuest(
  order1 = 4,
  order2 = 5,
  searchDegree = 5
)

object SeptagridPlushieQuest extends HedronQuest(
  order1 = 3,
  order2 = 7,
  searchDegree = 7
)

class HedronQuest(
  order1: Int,
  order2: Int,
  searchDegree: Int
) extends App:
  val group = Permutation.symmetricGroup(searchDegree)
  given Group[Permutation] = group
  private def elementsOfOrder(n: Int) =
    group.elements.filter:
      _.order == n
  private def conjugacyRepresentatives(elements: Set[Permutation]): Set[Permutation] =
    elements.map:
      _.canonicalConjugate
  val order1s = elementsOfOrder(order1)
  val order1sReduced = conjugacyRepresentatives(order1s)
  val order2s = elementsOfOrder(order2)
  val order2sReduced = conjugacyRepresentatives(order2s)
  println(s"${order1s.size} -> ${order1sReduced.size} elements of order $order1")
  println(s"${order2s.size} -> ${order2sReduced.size} elements of order $order2")
  val (loop1: Set[Permutation], loop2: Set[Permutation]) =
    if (order1sReduced.size * order2s.size) < (order1s.size * order2sReduced.size) then
      println(s"Winnowing $order1")
      (order1sReduced, order2s)
    else
      println(s"Winnowing $order2")
      (order1s, order2sReduced)

  case class Hedron(
    f: Permutation,
    v: Permutation,
    subgroup: group.Subgroup
  ):
    val order: Int =
      subgroup.order
  private val hedrons: Set[Hedron] =
    for
      f <- loop1
      v <- loop2
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

object PentagridPlushie extends PlushiePlayground(
  f = Permutation(0, 2, 3, 4, 1),
  v = Permutation(2, 4, 1, 0, 3)
)

object SeptagridPlushie extends PlushiePlayground(
  f = Permutation(6, 3, 1, 2, 0, 5, 4),
  v = Permutation(1, 2, 3, 4, 5, 6, 0)
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
  def rightCosetsOf(subgroup: plushieGroup.Subgroup): Seq[Coset] =
    plushieGroup.elements.map: g =>
      multiplyCoset(subgroup.elements, g)
    .toSeq
  def incident(c1: Coset, c2: Coset): Boolean =
    (c1 intersect c2).nonEmpty
  val fGroup: plushieGroup.Subgroup = plushieGroup.generateSubgroup(f)
  val vGroup: plushieGroup.Subgroup = plushieGroup.generateSubgroup(v)
  val eGroup: plushieGroup.Subgroup = plushieGroup.generateSubgroup(e)
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
    plushieGroup.elements.map: g =>
      multiplyCorner(sampleCorner, g)
  if cornerCoset.size == plushieGroup.order then
    println("Group acts torsorially on corners")
  else
    println("Not torsorial :(")
  def checkIncidences(
    subg1: plushieGroup.Subgroup,
    subg2: plushieGroup.Subgroup,
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

  println(s"χ(faceGraph) = ${faceGraph.chromaticNumber}")
  println(s"χ(vertexGraph) = ${vertexGraph.chromaticNumber}")
  println(s"χ(edgeGraph) = ${edgeGraph.chromaticNumber}")

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
