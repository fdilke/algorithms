package com.fdilke.apps

import com.fdilke.algebra.permutation.GroupSugar.*
import com.fdilke.algebra.permutation.{Group, GroupSugar, Permutation}
import com.fdilke.backtrack.node.coloring.Graph

// Attempt to find permutations v, f, e of order 5, 4, 2 respectively with fv = e
// These then form the generators of a finite homomorphic image of the von Dyck group D(5, 4, 2),
// from which we can extract an orientable Buekenhout geometry representing an abstract regular polytope

object KeyringPlushieQuest extends HedronQuest(
  faceSize = 4,
  vertexDegree = 8,
  searchDegree = 8
)

object PentagridPlushieQuest extends HedronQuest(
  faceSize = 5,
  vertexDegree = 4,
  searchDegree = 5
)

object SeptagridPlushieQuest extends HedronQuest(
  faceSize = 7,
  vertexDegree = 3,
  searchDegree = 7
)

object OctagridPlushieQuest extends HedronQuest(
  faceSize = 8,
  vertexDegree = 3,
  searchDegree = 8
)

object NonagridPlushieQuest extends HedronQuest(
  faceSize = 9,
  vertexDegree = 3,
  searchDegree = 9
)

class HedronQuest(
   faceSize: Int,
   vertexDegree: Int,
   searchDegree: Int
) extends App:
  Plushie.lookup(faceSize, vertexDegree).match
    case Some(plushie) => plushie
    case None =>
      Plushie.search(faceSize, vertexDegree, searchDegree)
  .investigate()

object Plushie:

  private val knownPlushies: Map[(Int, Int), Plushie] = Map(
      (5, 4) -> Plushie(
        f = Permutation(4, 0, 1, 2, 3),
        v = Permutation(0, 2, 3, 4, 1),
        name = "Pentaplushie"
      ),
      (7, 3) -> Plushie(
        f = Permutation(1, 2, 3, 4, 5, 6, 0),
        v = Permutation(2, 0, 1, 6, 4, 3, 5),
        name = "Septaplushie"
      ),
      (8, 3) -> Plushie(
        f = Permutation(1, 2, 3, 4, 5, 6, 7, 0),
        v = Permutation(7, 1, 0, 6, 3, 5, 4, 2),
        name = "Octoplushie"
      )
    )

  def lookup(
    faceSize: Int,
    vertexDegree: Int,
  ): Option[Plushie] =
    knownPlushies.get:
      (faceSize, vertexDegree)

  def search(
    faceSize: Int,
    vertexDegree: Int,
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
    val vCandidatesFull = elementsOfOrder(vertexDegree)
    val vCandidatesReduced = conjugacyRepresentatives(vCandidatesFull)
    val fCandidatesFull = elementsOfOrder(faceSize)
    val fCandidatesReduced = conjugacyRepresentatives(fCandidatesFull)
    println(s"${vCandidatesFull.size} -> ${vCandidatesReduced.size} elements of order $vertexDegree")
    println(s"${fCandidatesFull.size} -> ${fCandidatesReduced.size} elements of order $faceSize")
    val (fCandidates: Set[Permutation], vCandidates: Set[Permutation]) =
      if (vCandidatesReduced.size * fCandidatesFull.size) < (vCandidatesFull.size * fCandidatesReduced.size) then
        println(s"Winnowing $vertexDegree")
        (fCandidatesFull, vCandidatesReduced)
      else
        println(s"Winnowing $faceSize")
        (fCandidatesReduced, vCandidatesFull)

    case class Hedron(
      f: Permutation,
      v: Permutation,
      subgroup: group.Subgroup
    ):
      val order: Int =
        subgroup.order
    val hedrons: Set[Hedron] =
      for
        f <- fCandidates
        v <- vCandidates
        e = f * v if e.order == 2
      yield
        val subgroup = group.generateSubgroup(f, v)
        Hedron(f, v, subgroup)
    val orders: Seq[Int] =
      hedrons.map { _.order }.toSeq.sorted
    println("Sorted orders: " + orders.mkString(","))
    val smallestOrder = orders.head
    val hedron: Hedron =
      hedrons.find:
        _.order == smallestOrder
      .getOrElse:
        throw new IllegalArgumentException("can't find smallest plushie")
    println("for the small plushie:")
    println("f = " + hedron.f)
    println("v = " + hedron.v)
    val schlafliName: String =
      s"{ $vertexDegree, $faceSize }"
    Plushie(f = hedron.f, v = hedron.v, name = schlafliName)

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
    val vertexes = rightCosetsOf(vGroup)
    val faces = rightCosetsOf(fGroup)
    val edges = rightCosetsOf(eGroup)
    println(s"${vertexes.size} vertexes")
    println(s"${edges.size} edges")
    println(s"${faces.size} faces")
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
      subgroup1: plushieGroup.Subgroup,
      subgroup2: plushieGroup.Subgroup,
    ): String =
      val theCount: Int =
        subgroup1.order /
          subgroup1.elements.intersect(subgroup2.elements).size
      if
        rightCosetsOf(subgroup1).forall: s1 =>
          rightCosetsOf(subgroup2).count: s2 =>
            incident(s1, s2)
          == theCount
      then
        theCount.toString
      else
        "?"

    println(s"Every face is incident to ${checkIncidences(fGroup, vGroup)} vertexes")
    println(s"Every face is incident to ${checkIncidences(fGroup, eGroup)} edges")
    println(s"Every vertex is incident to ${checkIncidences(vGroup, fGroup)} faces")
    println(s"Every vertex is incident to ${checkIncidences(vGroup, eGroup)} edges")
    println(s"Every edge is incident to ${checkIncidences(eGroup, vGroup)} vertexes")
    println(s"Every edge is incident to ${checkIncidences(eGroup, fGroup)} faces")

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
