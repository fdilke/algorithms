package com.fdilke.apps

import com.fdilke.algebra.permutation.GroupSugar.*
import com.fdilke.algebra.permutation.{Group, GroupSugar, Permutation}
import com.fdilke.backtrack.node.coloring.Graph

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

object OctagridPlushieQuest extends HedronQuest(
  order1 = 3,
  order2 = 8,
  searchDegree = 8
)

object NonagridPlushieQuest extends HedronQuest(
  order1 = 3,
  order2 = 9,
  searchDegree = 9
)

class HedronQuest(
  order1: Int,
  order2: Int,
  searchDegree: Int
) extends App:
  Plushie.lookup(order1, order2).match
    case Some(plushie) => plushie
    case None =>
      Plushie.search(order1, order2, searchDegree)
  .investigate()

object Plushie:

  private val knownPlushies: Map[(Int, Int), Plushie] = Map(
      (4, 5) -> Plushie(
        f = Permutation(0, 2, 3, 4, 1),
        v = Permutation(2, 4, 1, 0, 3),
        name = "Pentaplushie"
      ),
      (3, 7) -> Plushie(
        f = Permutation(6, 3, 1, 2, 0, 5, 4),
        v = Permutation(1, 2, 3, 4, 5, 6, 0),
        name = "Septaplushie"
      ),
      (3, 8) -> Plushie(
        f = Permutation(6, 4, 1, 3, 2, 0, 5, 7),
        v = Permutation(1, 2, 3, 4, 5, 6, 7, 0),
        name = "Octoplushie"
      )
    )
// TODO: Schlafli the wrong way round, fix

  def lookup(
    order1: Int,
    order2: Int,
  ): Option[Plushie] =
    knownPlushies.get:
      (order1, order2)

  def search(
    order1: Int,
    order2: Int,
    searchDegree: Int
  ): Plushie =
    val group = Permutation.symmetricGroup(searchDegree)
    given Group[Permutation] = group
    def elementsOfOrder(n: Int) =
      group.elements.filter:
        _.order == n
    def conjugacyRepresentatives(elements: Set[Permutation]): Set[Permutation] =
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
    val hedrons: Set[Hedron] =
      for
        f <- loop1
        v <- loop2
        e = f * v if e.order == 2
      yield
        val subgroup = group.generateSubgroup(f, v)
        Hedron(f, v, subgroup)
    val orders: Seq[Int] =
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
    val schlafliName: String =
      s"{ $order1, $order2 }"
    Plushie(f = plushie.f, v = plushie.v, name = schlafliName)

class Plushie(
  val f: Permutation,
  val v: Permutation,
  name: String
):
  def investigate(): Unit =
    val group: Group[Permutation] = Permutation.symmetricGroup(
      Math.max(f.degree, v.degree)
    )
    given Group[Permutation] = group
    val e = f * v
    assert(e.order == 2)
    val plushieGroup: group.Subgroup =
      group.generateSubgroup(f, v)
    println(s"Plushie group order = ${plushieGroup.order}")
    println(s"Plushie group simple = ${plushieGroup.simple}")
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
          subg1.elements.intersect(subg2.elements).size
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
    println(s"Every edge is incident to ${checkIncidences(eGroup, vGroup)} faces")

    def edgesAdjacent(e1: Coset, e2: Coset): Boolean =
      vertexes.exists: v =>
        incident(v, e1) && incident(v, e2)

    def adjacentFaceMultiplicity(f1: Coset, f2: Coset): Int =
      edges.count: edge =>
        incident(edge, f1) && incident(edge, f2)

    val faceAdjacencyMultiplicities: Map[Set[Int], Int] =
      val multiplicities: Seq[Seq[(Set[Int], Int)]] =
        for
          (faceI, i) <- faces.zipWithIndex
          (faceJ, j) <- faces.take(i).zipWithIndex
          multiplicity = adjacentFaceMultiplicity(faceI, faceJ)
        yield
          if multiplicity > 0 then
            Seq((Set(i, j), multiplicity))
          else
            Seq.empty[(Set[Int], Int)]
      multiplicities.flatten.toMap

    val faceAdjacencies: Seq[(Int, Int)] =
      faceAdjacencyMultiplicities.keySet.toSeq.collect: set =>
        set.toSeq match
          case Seq(a, b) => (a, b)

    def faceLabel(i: Int): String =
      ('A'.toInt + i).toChar.toString
    def edgeLabel(i: Int): String =
      ('a'.toInt + i).toChar.toString
    def vertexLabel(i: Int): String =
      i.toString

    println("# face adjacencies: " + faceAdjacencies.size)
    for
      i <- faces.indices
    do
      print(s"${faceLabel(i)}: ")
      for
        j <- faces.indices if faceAdjacencies.contains(i, j) || faceAdjacencies.contains(j, i)
        multiplicity = faceAdjacencyMultiplicities(Set(i, j))
      do
        val indicator =
          if (multiplicity > 1)
            s"x$multiplicity"
          else
            ""
        print(s"${faceLabel(j)}$indicator ")
      println("")

    println("# faces: " + faces.size)
    for
      (face, i) <- faces.zipWithIndex
    do
      print(s"${faceLabel(i)}: ")
      val incidentEdges: Seq[Int] =
        for
          (edge, j) <- edges.zipWithIndex if incident(edge, face)
        yield j
      def nextVertexEdge(
        vertexEdge: (Int, Int)
      ): (Int, Int) =
        val (aVertex: Int, anEdge: Int) = vertexEdge
        val nextEdge: Int =
          incidentEdges.find: e =>
            e != anEdge &&
              edgesAdjacent(edges(e), edges(anEdge)) &&
              !incident(edges(e), vertexes(aVertex))
          .getOrElse:
            throw new IllegalArgumentException("can't find next edge")
        val nextVertex: Int =
          vertexes.zipWithIndex.find: (vertex, _) =>
            incident(vertex, edges(nextEdge)) &&
            incident(vertex, edges(anEdge))
          .map ( _. _2 )
          .getOrElse:
            throw new IllegalArgumentException("can't find next vertex")
        (nextVertex, nextEdge)
      val firstEdge: Int = incidentEdges.head
      val firstVertex =
        vertexes.indices.find: v =>
          incident(vertexes(v), edges(firstEdge))
        .getOrElse:
          throw new IllegalArgumentException("can't find first vertex")
      val cycleVertexesEdges: Seq[(Int, Int)] =
        Seq.iterate[(Int, Int)](
          (firstVertex, firstEdge),
          incidentEdges.size
        ):
          nextVertexEdge
      println:
        cycleVertexesEdges.map: (v, e) =>
          vertexLabel(v) + "-" + edgeLabel(e)
        .mkString("-")

    println("# edges: " + edges.size)
    for
      (edge, i) <- edges.zipWithIndex
    do
      print(s"${edgeLabel(i)}: ")
      val incidentVertexes: Seq[Int] =
        for
          (vertex, j) <- vertexes.zipWithIndex if incident(vertex, edge)
        yield j
      println:
        incidentVertexes.map(vertexLabel).mkString("--")

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

    for
      (graphName: String, graph: Graph) <- Seq(
        "faceGraph" -> faceGraph,
        "edgeGraph" -> edgeGraph,
        "vertexGraph" -> vertexGraph
      )
    do
      print(s"χ($graphName) = ")
      println(s"${graph.chromaticNumber}")

      print(s"$graphName is distance-transitive:")
      println(graph.distanceTransitive)

      print(s"$graphName is distance-regular:")
      println(graph.distanceRegular)

      print(s"$graphName is Cayley:")
      println(graph.cayley)
