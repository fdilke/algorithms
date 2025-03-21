package com.fdilke.backtrack.node.coloring

import com.fdilke.algebra.permutation.{Group, GroupVerifier, Permutation}
import com.fdilke.backtrack.node.coloring.Graph._
import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

import scala.util.Random

class GraphSpec extends FunSuite:
  private val runSlowTests: Boolean = false

  test("can construct an adjacency table from a list of edges"):
    Graph((0, 1), (1,2)).adjacencyTable is Seq(
      Seq(false, true, false),
      Seq(true, false, true),
      Seq(false, true, false),
    )

  test("can construct a list of edges from an adjacency table"):
    Graph(
      false, true, false,
      true, false, true,
      false, true, false
    ).edges is Seq(
      (0, 1), (1, 2)
    )

  test("can construct a list of edges from an unpacked adjacency table"):
    Graph(
      false, true, false,
      true, false, true,
      false, true, false
    ).edges is Seq(
      (0, 1), (1, 2)
    )

  test("Can sort the vertices of a graph by descending degree"):
    val (order, inverse): (Seq[Int], Seq[Int]) =
      Graph(
        false, true, false,
        true,  false, true,
        false, true, false
      ).sortByDescDegree()
    order.size is 3
    inverse.size is 3
    val diagonal: Seq[Int] = 0 until 3
    diagonal.map { i => order(inverse(i)) } is diagonal
    diagonal.map { i => inverse(order(i)) } is diagonal
    order.head is 1

  test("Can sort the vertices of a graph by descending degree (2)"):
    val (order, inverse): (Seq[Int], Seq[Int]) =
      Graph(
        false, true, true, false, false,
        true, false, false, true, true,
        true, false, false, false, true,
        false, true, false, false, true,
        false, true, true, true, false
      ).sortByDescDegree()
    order.size is 5
    inverse.size is 5
    val diagonal: Seq[Int] = 0 until 3
    diagonal.map { i => order(inverse(i)) } is diagonal
    diagonal.map { i => inverse(order(i)) } is diagonal
    order.take(2) isSet Set(1,4)

  test("check a graph is antireflexive"):
    Graph(
      false, true, false,
      true, false, true,
      false, true, false
    ).checkAntireflexive()
    intercept[IllegalArgumentException]:
      Graph(
        true, false,
        true, false
      ).checkAntireflexive()

  test("check a graph is antireflexive"):
    Graph(
      false, true, false,
      true, false, true,
      false, true, false
    ).checkSymmetric()
    intercept[IllegalArgumentException]:
      Graph(
        true, false,
        true, false
      ).checkSymmetric()

  test("Can compute the last vertex from pairs"):
    lastVertexFromPairs() is -1
    lastVertexFromPairs(0 -> 1) is 1
    lastVertexFromPairs(7 -> 8, 0 -> 1) is 8
    lastVertexFromPairs(2 -> 3, 4 -> 0, 5 -> 6, 0 -> 1) is 6

  test("Can construct an adjacency table from pairs"):
    adjacencyTableFromPairs() is Seq()
    adjacencyTableFromPairs((0, 1)) is Seq(Seq(false, true), Seq(true, false))
    adjacencyTableFromPairs((1, 2)) is Seq(
      Seq(false, false, false),
      Seq(false, false, true),
      Seq(false, true, false)
    )
    adjacencyTableFromPairs((0, 1), (1, 2)) is Seq(
      Seq(false, true, false),
      Seq(true,  false, true),
      Seq(false, true, false)
    )
    adjacencyTableFromPairs((0, 3), (1, 2)) is Seq(
      Seq(false, false, false, true),
      Seq(false, false, true, false),
      Seq(false, true, false, false),
      Seq(true,  false, false, false)
    )

  test("Algorithm for creating random planar graphs via layering: exclude freak cases"):
    intercept[IllegalArgumentException]:
      addLayer(Seq(0), 1, -1, 0)
    .getMessage is "startIndex < 0"
    intercept[IllegalArgumentException]:
      addLayer(Seq(0), 1, 1, 0)
    .getMessage is "startIndex >= circle size"
    intercept[IllegalArgumentException]:
      addLayer(Seq(0), 1, 0, 0)
    .getMessage is "coverLength <= 0"
    intercept[IllegalArgumentException]:
      addLayer(Seq(0), 1, 0, 2)
    .getMessage is "coverLength > circle size"
    intercept[IllegalArgumentException]:
      addLayer(Seq(0), 1, 1, 1)
    .getMessage is "startIndex >= circle size"

  test("Algorithm for layering: low order cases with wrapping"):
    addLayer(Seq(0), 1, 0, 1) is (Seq(1, 0, 0), Seq(0 -> 1))
    addLayer(Seq(0), 2, 0, 1) is (Seq(2, 0, 0), Seq(0 -> 2))

  test("Algorithm for layering: low order cases without wrapping"):
    addLayer(Seq(1, 2), 3, 0, 1) is (Seq(1, 3, 1, 2), Seq(1 -> 3))
    addLayer(Seq(1, 2), 0, 0, 1) is (Seq(1, 0, 1, 2), Seq(1 -> 0))

  test("Algorithm for layering: non-wrapping cases of varying coverLengths"):
    addLayer(Seq(3, 4, 5, 6, 7, 8, 9), 0, 3, 1) is (
      Seq(3, 4, 5, 6, 0, 6, 7, 8, 9),
      Seq(6 -> 0)
    )
    addLayer(Seq(3, 4, 5, 6, 7, 8, 9), 0, 3, 2) is (
      Seq(3, 4, 5, 6, 0, 7, 8, 9),
      Seq(6 -> 0, 7 -> 0)
    )

  test("Algorithm for layering: wrapping cases of varying coverLengths"):
    addLayer(Seq(3, 4, 5, 6, 7, 8, 9), 0, 4, 4) is (
      Seq(0, 3, 4, 5, 6, 7),
      Seq(7 -> 0, 8 -> 0, 9 -> 0, 3 -> 0)
    )
    addLayer(Seq(3, 4, 5, 6, 7, 8, 9), 0, 4, 3) is (
      Seq(0, 9, 3, 4, 5, 6, 7),
      Seq(7 -> 0, 8 -> 0, 9 -> 0)
    )

  test("Algorithm for layering: some other cases"):
    addLayer(Seq(3, 4, 5, 6), 7, 1, 1) is (Seq(3, 4, 7, 4, 5, 6), Seq(4 -> 7))
    addLayer(Seq(3, 4), 7, 0, 1) is (Seq(3, 7, 3, 4), Seq(3 -> 7))
    addLayer(Seq(3, 4, 5, 6), 7, 0, 1) is (Seq(3, 7, 3, 4, 5, 6), Seq(3 -> 7))
    addLayer(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9), 0, 2, 3) is (
      Seq(1, 2, 3, 0, 5, 6, 7, 8, 9),
      Seq(3 -> 0, 4 -> 0, 5 -> 0)
    )
    addLayer(Seq(3, 4, 5, 6, 7, 8, 9), 0, 1, 1) is(
      Seq(3, 4, 0, 4, 5, 6, 7, 8, 9),
      Seq(4 -> 0)
    )
    addLayer(Seq(3, 4, 5, 6, 7, 8), 0, 0, 1) is (Seq(3, 0, 3, 4, 5, 6, 7, 8), Seq(3 -> 0))
    addLayer(Seq(3, 4), 7, 0, 1) is (Seq(3, 7, 3, 4), Seq(3 -> 7))
    addLayer(Seq(3, 4, 5), 7, 1, 1) is (Seq(3, 4, 7, 4, 5), Seq(4 -> 7))
    addLayer(Seq(3, 4, 5), 7, 1, 2) is (Seq(7, 5, 3, 4), Seq(4 -> 7, 5 -> 7))
    addLayer(Seq(3, 4, 5), 7, 1, 3) is (Seq(7, 3, 4), Seq(4 -> 7, 5 -> 7, 3 -> 7))

  test("Generating random planar graphs"):
    val size = 10
    val graph: Graph =
      randomPlanar(size, Random(0L))
    graph.numVertices is size
    graph.adjacencyTable.size is size
    (graph.adjacencyTable forall:
      _.size == size
    ) is true
    ColorGraphLoop(
      4,
      graph
    ).isDefined is true

  test("torus(0, 0) is empty"):
    torus(0, 0).numVertices is 0

  test("torus(0, 3) is empty"):
    torus(0, 3).numVertices is 0

  test("torus(3, 0) is empty"):
    torus(3, 0).numVertices is 0

  test("torus(1, 1) has 1 vertex"):
    torus(1, 1).edges is Seq(0 -> 0)

  test("torus(3, 2) has 6 vertices and 12 edges"):
    val torus32: Graph = torus(3, 2)
    torus32.edges.flatMap:
      case (v: Int, w: Int) => Seq(v, w)
    .distinct.sorted is (0 until 6)
    torus32.edges.size is 12
    torus32.edges.sorted is Seq(
      (0, 1), (0, 2),
      (1, 0), (1, 3),
      (2, 3), (2, 4),
      (3, 2), (3, 5),
      (4, 0), (4, 5),
      (5, 1), (5, 4)
    )

  // todo: add more adequate torus tests... but this is better than nothing
  test("The first odd graph is empty (should really be a singleton)"):
    oddGraph(1).numVertices is 0

  test("The second odd graph is a triangle"):
    oddGraph(2).edges.sorted is Seq(
      (0, 1), (0, 2), (1, 2)
    )

  test("The second odd graph is Petersen"):
    oddGraph(3).edges.sorted is Seq(
      (0, 7), (0, 8), (0, 9),
      (1, 5), (1, 6), (1, 9),
      (2, 4), (2, 6), (2, 8),
      (3, 4), (3, 5), (3, 7),
      (4, 9), (5, 8), (6, 7)
    )

  test("can construct complete bipartite graphs"):
    completeBipartite(2, 3).edges isSet Set(
      (0, 2), (1, 2),
      (0, 3), (1, 3),
      (0, 4), (1, 4)
    )

  test("calculate the neighbors of a node"):
    val graph: Graph =
      Graph(
        (1, 2), (3, 4), (4, 5), (4, 6), (5, 6)
      )
    graph.neighborsOf(0) is Seq()
    graph.neighborsOf(1) is Seq(2)
    graph.neighborsOf(2) is Seq(1)
    graph.neighborsOf(3) is Seq(4)
    graph.neighborsOf(4) is Seq(3, 5, 6)
    graph.neighborsOf(5) is Seq(4, 6)
    graph.neighborsOf(6) is Seq(4, 5)
    
  test("calculate distance maps"):
    val graph: Graph =
      Graph(
        (1, 2), (3, 4), (4, 5)
      )
    graph.distanceMap(0) is Map(0 -> 0)
    graph.distanceMap(1) is Map(1 -> 0, 2 -> 1)
    graph.distanceMap(2) is Map(1 -> 1, 2 -> 0)
    graph.distanceMap(3) is Map(3 -> 0, 4 -> 1, 5 -> 2)
    graph.distanceMap(4) is Map(3 -> 1, 4 -> 0, 5 -> 1)
    graph.distanceMap(5) is Map(3 -> 2, 4 -> 1, 5 -> 0)

  test("calculate the complete set of distance maps"):
    val graph: Graph =
      Graph((0, 1))
    graph.distanceMap(0) is Map(0 -> 0, 1 -> 1)
    graph.distanceMap(1) is Map(0 -> 1, 1 -> 0)
    graph.distanceMaps is Seq(
      Seq(0, 1),
      Seq(1, 0)
    )

  test("enumerate single point extensions of a partial automorphism"):
    val graph: Graph =
      Graph(
        (0, 1), (0, 2)
      )
    graph.singlePointExtensions(Seq()) is Iterable(
      Seq(0), Seq(1), Seq(2)
    )
    graph.singlePointExtensions(Seq(0)) is Iterable(
      Seq(0, 1), Seq(0, 2)
    )
    graph.singlePointExtensions(Seq(0, 1)) is Iterable(
      Seq(0, 1, 2)
    )
    graph.singlePointExtensions(Seq(0, 2)) is Iterable(
      Seq(0, 2, 1)
    )
    intercept[IllegalArgumentException]:
      graph.singlePointExtensions(Seq(0, 1, 2))

  test("enumerate full extensions of a partial automorphism"):
    val graph: Graph =
      Graph(
        (0, 1), (0, 2)
      )
    graph.fullExtensions(Seq()) isSet Seq(
      Seq(0, 2, 1), Seq(0, 1, 2)
    )
    graph.fullExtensions(Seq(0)) isSet Seq(
      Seq(0, 2, 1), Seq(0, 1, 2)
    )
    graph.fullExtensions(Seq(0, 1)) isSet Seq(
      Seq(0, 1, 2)
    )
    graph.fullExtensions(Seq(0, 2)) isSet Seq(
      Seq(0, 2, 1)
    )
    graph.fullExtensions(Seq(0, 2, 1)) isSet Seq(
      Seq(0, 2, 1)
    )

  test("enumerate full extensions of a partial automorphism (2)"):
    val graph: Graph =
      Graph(
        (0, 1), (1, 2), (2, 3), (3, 1)
      )
    graph.fullExtensions(Seq(0, 1)) isSet Seq(
      Seq(0, 1, 3, 2), Seq(0, 1, 2, 3)
    )
    graph.fullExtensions(Seq(0, 2)) isSet Seq.empty
    graph.fullExtensions(Seq(0, 1, 2)) isSet Set(
      Seq(0, 1, 2, 3)
    )
    graph.fullExtensions(Seq(0, 1, 3)) isSet Set(
      Seq(0, 1, 3, 2)
    )
    graph.fullExtensions(Seq()) isSet Set(
      Seq(0, 1, 3, 2), Seq(0, 1, 2, 3)
    )

  test("enumerate full extensions of a partial automorphism (3)"):
    val graph: Graph =
      petersen
    graph.numVertices is 10
    graph.edges.size is 15
    graph.fullExtensions(Seq()).size is 120

  test("enumerate single point extensions of a partial automorphism via maps"):
    val graph: Graph =
      Graph(
        (0, 1), (0, 2)
      )
    graph.singlePointExtensionsMap(Map()) isSet Set(
      Map(0 -> 0), Map(0 -> 1), Map(0 -> 2)
    )
    graph.singlePointExtensionsMap(Map(0 -> 0)) isSet Set(
      Map(0 -> 0, 1 -> 1), Map(0 -> 0, 1 -> 2)
    )
    graph.singlePointExtensionsMap(Map(1 -> 2)) isSet Set(
      Map(0 -> 0, 1 -> 2)
    )
    graph.singlePointExtensionsMap(Map(2 -> 1)) isSet Set(
      Map(0 -> 0, 2 -> 1)
    )
    graph.singlePointExtensionsMap(Map(0 -> 0, 1 -> 1)) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 2)
    )
    graph.singlePointExtensionsMap(Map(0 -> 0, 1 -> 2)) isSet Set(
      Map(0 -> 0, 1 -> 2, 2 -> 1)
    )
    intercept[IllegalArgumentException]:
      graph.singlePointExtensionsMap(Map(0 -> 0, 1 -> 1, 2 -> 2))

  test("enumerate full extensions of a partial automorphism, by maps"):
    val graph: Graph =
      Graph(
        (0, 1), (0, 2)
      )
    graph.fullExtensionsMap(Map()) isSet Seq(
      Map(0 -> 0, 1 -> 2, 2 -> 1), Map(0 -> 0, 1 -> 1, 2 -> 2)
    )
    graph.fullExtensionsMap(Map(0 -> 0)) isSet Seq(
      Map(0 -> 0, 1 -> 2, 2 -> 1), Map(0 -> 0, 1 -> 1, 2 -> 2)
    )
    graph.fullExtensionsMap(Map(0 -> 0, 1 -> 1)) isSet Seq(
      Map(0 -> 0, 1 -> 1, 2 -> 2)
    )
    graph.fullExtensionsMap(Map(1 -> 1)) isSet Seq(
      Map(0 -> 0, 1 -> 1, 2 -> 2)
    )
    graph.fullExtensionsMap(Map(2 -> 2)) isSet Seq(
      Map(0 -> 0, 1 -> 1, 2 -> 2)
    )
    graph.fullExtensionsMap(Map(0 -> 0, 1 -> 2)) isSet Seq(
      Map(0 -> 0, 1 -> 2, 2 -> 1)
    )
    graph.fullExtensionsMap(Map(1 -> 2)) isSet Seq(
      Map(0 -> 0, 1 -> 2, 2 -> 1)
    )
    graph.fullExtensionsMap(Map(2 -> 1)) isSet Seq(
      Map(0 -> 0, 1 -> 2, 2 ->1)
    )
    graph.fullExtensionsMap(Map(0 -> 0, 1 -> 2, 2 -> 1)) isSet Seq(
      Map(0 -> 0, 1 -> 2, 2 ->1)
    )

  test("enumerate full extensions of a partial automorphism, by maps (2)"):
    val graph: Graph =
      Graph(
        (0, 1), (1, 2), (2, 3), (3, 1)
      )
    graph.fullExtensionsMap(Map(0 -> 0, 1 -> 1)) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 3, 3 -> 2), Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
    )
    graph.fullExtensionsMap(Map(1 -> 1)) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 3, 3 -> 2), Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
    )
    graph.fullExtensionsMap(Map(0 -> 0, 1 -> 2)) isSet Set.empty
    graph.fullExtensionsMap(Map(1 -> 2)) isSet Set.empty
    graph.fullExtensionsMap(Map(2 -> 2)) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
    )
    graph.fullExtensionsMap(Map(3 -> 3)) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
    )
    graph.fullExtensionsMap(Map(0 -> 0, 1 -> 1, 2 -> 2)) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
    )
    graph.fullExtensionsMap(Map(0 -> 0, 1 -> 1, 2 -> 3)) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 3, 3 -> 2)
    )
    graph.fullExtensionsMap(Map()) isSet Set(
      Map(0 -> 0, 1 -> 1, 2 -> 3, 3 -> 2), Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)
    )

  test("enumerate full extensions of a partial automorphism, by maps (3)"):
    val graph: Graph =
      petersen
    graph.numVertices is 10
    graph.edges.size is 15
    graph.fullExtensionsMap(Map()).size is 120

  test("vital stats of the empty graph"):
    emptyGraph.edges.isEmpty is true
    emptyGraph.vertices.isEmpty is true
    emptyGraph.adjacencyTable.isEmpty is true

  test("tell if a graph is distance-transitive"):
    emptyGraph.distanceTransitive is true
    onePointGraph.distanceTransitive is true
    Graph(
      (0, 1), (1, 2), (2, 3), (3, 1)
    ).distanceTransitive is false
    Graph(
      (0, 1)
    ).distanceTransitive is true
    Graph(
      (0, 1), (1, 2)
    ).distanceTransitive is false
    Graph(
      (0, 1), (1, 2), (2, 0)
    ).distanceTransitive is true
    if runSlowTests then
      petersen.distanceTransitive is true
      heawood.distanceTransitive is true
      pappus.distanceTransitive is true
      shrikhande.distanceTransitive is false
      cubicalGraph.distanceTransitive is true
      dodecahedralGraph.distanceTransitive is true
      icosahedralGraph.distanceTransitive is true
    tetrahedralGraph.distanceTransitive is true
    octahedralGraph.distanceTransitive is true
    completeBipartite(2, 3).distanceTransitive is false
    completeBipartite(3, 3).distanceTransitive is true

  test("can measure the diameter of a graph"):
    onePointGraph.diameter is 0
    Graph((0, 1)).diameter is 1
    Graph((0, 1), (1, 2)).diameter is 2
    Graph(
      (0, 1), (1, 2), (2, 0)
    ).diameter is 1
    petersen.diameter is 2
    heawood.diameter is 3
    pappus.diameter is 4
    shrikhande.diameter is 2
    cubicalGraph.diameter is 3
    tetrahedralGraph.diameter is 1
    octahedralGraph.diameter is 2
    dodecahedralGraph.diameter is 5
    icosahedralGraph.diameter is 3
    completeBipartite(2, 3).diameter is 2
    completeBipartite(3, 3).diameter is 2

  test("can calculate local intersection arrays"):
    onePointGraph.localIntersectionArray(0) is Some(Seq(), Seq())
    Graph((0, 1)).localIntersectionArray(0) is Some(Seq(1), Seq(1))
    Graph((0, 1), (1, 2)).localIntersectionArray(0) is Some(Seq(1, 1), Seq(1,1))
    Graph((0, 1), (1, 2)).localIntersectionArray(1) is Some(Seq(2), Seq(1))
    Graph((0, 1), (1, 2), (2, 3), (1, 4)).localIntersectionArray(0) is None
    Graph((0, 1), (1, 2), (2, 3), (1, 4)).localIntersectionArray(1) is None
    Graph((0, 1), (0, 2), (0, 3)).localIntersectionArray(0) is Some(Seq(3), Seq(1))
    Graph((0, 1), (0, 2), (0, 3)).localIntersectionArray(2) is Some(Seq(1, 2), Seq(1, 1))
    tetrahedralGraph.localIntersectionArray(0) is Some(Seq(3), Seq(1))
    octahedralGraph.localIntersectionArray(0) is Some(Seq(4, 1), Seq(1, 4))
    petersen.localIntersectionArray(1) is Some(Seq(3, 2), Seq(1, 1))
    dodecahedralGraph.localIntersectionArray(2) is Some(Seq(3,2,1,1,1), Seq(1,1,1,2,3))
    icosahedralGraph.localIntersectionArray(2) is Some(Seq(5,2,1), Seq(1,2,5))
    cubicalGraph.localIntersectionArray(3) is Some(Seq(3,2,1), Seq(1,2,3))
    heawood.localIntersectionArray(4) is Some(Seq(3,2,2), Seq(1,1,3))
    pappus.localIntersectionArray(5) is Some(Seq(3,2,2,1), Seq(1,1,2,3))
    shrikhande.localIntersectionArray(6) is Some(Seq(6, 3), Seq(1,2))

  test("can calculate global intersection arrays"):
    emptyGraph.intersectionArray is None
    onePointGraph.intersectionArray is Some(Seq(), Seq())
    Graph((0, 1)).intersectionArray is Some(Seq(1), Seq(1))
    Graph((0, 1), (1, 2)).intersectionArray is None
    Graph((0, 1), (1, 2), (2, 3), (1, 4)).intersectionArray is None
    Graph((0, 1), (0, 2), (0, 3)).intersectionArray is None
    tetrahedralGraph.intersectionArray is Some(Seq(3), Seq(1))
    octahedralGraph.intersectionArray is Some(Seq(4, 1), Seq(1, 4))
    petersen.intersectionArray is Some(Seq(3, 2), Seq(1, 1))
    dodecahedralGraph.intersectionArray is Some(Seq(3,2,1,1,1), Seq(1,1,1,2,3))
    icosahedralGraph.intersectionArray is Some(Seq(5, 2, 1), Seq(1, 2, 5))
    cubicalGraph.intersectionArray is Some(Seq(3,2,1), Seq(1,2,3))
    heawood.intersectionArray is Some(Seq(3,2,2), Seq(1,1,3))
    pappus.intersectionArray is Some(Seq(3,2,2,1), Seq(1,1,2,3))
    shrikhande.intersectionArray is Some(Seq(6, 3), Seq(1,2))

  test("can tell if a graph is distance-regular"):
    emptyGraph.distanceRegular is false
    onePointGraph.distanceRegular is true
    Graph((0, 1)).distanceRegular is true
    Graph((0, 1), (1, 2)).distanceRegular is false
    Graph((0, 1), (1, 2), (2, 3), (1, 4)).distanceRegular is false
    Graph((0, 1), (0, 2), (0, 3)).distanceRegular is false
    tetrahedralGraph.distanceRegular is true
    octahedralGraph.distanceRegular is true
    petersen.distanceRegular is true
    dodecahedralGraph.distanceRegular is true
    icosahedralGraph.distanceRegular is true
    cubicalGraph.distanceRegular is true
    heawood.distanceRegular is true
    pappus.distanceRegular is true
    shrikhande.distanceRegular is true

  test("can tell if a graph is connected"):
    emptyGraph.connected is false
    onePointGraph.connected is true
    Graph(
      false, false,
      false, false
    ).connected is false
    Graph((0, 1), (2, 3)).connected is false
    Graph((0, 1), (1, 2)).connected is true
    Graph((0, 1), (1, 3)).connected is false
    Graph((0, 1), (1, 2)).connected is true
    Graph((0, 1), (1, 2), (2, 3), (1, 4)).connected is true
    Graph((0, 2), (0, 3)).connected is false
    tetrahedralGraph.connected is true
    octahedralGraph.connected is true
    petersen.connected is true
    dodecahedralGraph.connected is true
    icosahedralGraph.connected is true
    cubicalGraph.connected is true
    heawood.connected is true
    pappus.connected is true
    shrikhande.connected is true

  test("can tell if a graph is vertex-transitive"):
    emptyGraph.vertexTransitive is false
    onePointGraph.vertexTransitive is true
    Graph((0, 1)).vertexTransitive is true
    Graph((0, 1), (1, 2)).vertexTransitive is false
    Graph((0, 1), (1, 2), (2, 3), (1, 4)).vertexTransitive is false
    Graph((0, 1), (0, 2)).vertexTransitive is false
    Graph((0, 1), (0, 2), (0, 3)).vertexTransitive is false
    tetrahedralGraph.vertexTransitive is true
    octahedralGraph.vertexTransitive is true
    petersen.vertexTransitive is true
    dodecahedralGraph.vertexTransitive is true
    icosahedralGraph.vertexTransitive is true
    cubicalGraph.vertexTransitive is true
    heawood.vertexTransitive is true
    pappus.vertexTransitive is true
    shrikhande.vertexTransitive is true
    completeBipartite(2, 3).vertexTransitive is false
    completeBipartite(3, 3).vertexTransitive is true

  private def checkGroup(
    graph : Graph,
    expectedAutos: Int
  ): Unit =
    val group: Group[Permutation] =
      graph.automorphisms
    // GroupVerifier.checkGroupOf(group)
    group.order is expectedAutos

  test("compute the automorphism group of a graph"):
    checkGroup(emptyGraph, 1)
    checkGroup(onePointGraph, 1)
    checkGroup(Graph((0, 1)), 2)
    checkGroup(Graph((0, 1), (1, 2)), 2)
    checkGroup(Graph((0, 1), (1, 2), (2, 3), (1, 4)), 2)
    checkGroup(Graph((0, 1), (1, 2), (2, 4), (1, 3), (3, 5), (5, 6)), 1)
    checkGroup(tetrahedralGraph, 24)
    checkGroup(octahedralGraph, 48)
    checkGroup(petersen, 120)
    checkGroup(dodecahedralGraph, 120)
    checkGroup(icosahedralGraph, 120)
    checkGroup(cubicalGraph, 48)
    checkGroup(heawood, 336)
    checkGroup(pappus, 216)
    checkGroup(shrikhande, 192)
    checkGroup(completeBipartite(2, 3), 12)
    checkGroup(completeBipartite(3, 3), 72)

  test("tell if a graph is Cayley"):
    emptyGraph.cayley is false
    onePointGraph.cayley is true
    Graph((0, 1)).cayley is true
    Graph((0, 1), (1, 2)).cayley is false
    tetrahedralGraph.cayley is true
    octahedralGraph.cayley is true
    petersen.cayley is false
    if runSlowTests then
      dodecahedralGraph.cayley is false
      icosahedralGraph.cayley is true
      cubicalGraph.cayley is true
      heawood.cayley is true
      pappus.cayley is true
      shrikhande.cayley is true
    completeBipartite(2, 3).cayley is false
    completeBipartite(3, 3).cayley is true

  test("can measure the chromatic number of a graph"):
    emptyGraph.chromaticNumber is 0
    onePointGraph.chromaticNumber is 1
    Graph((0, 1)).chromaticNumber is 2
    Graph((0, 1), (1, 2)).chromaticNumber is 2
    Graph(
      (0, 1), (1, 2), (2, 0)
    ).chromaticNumber is 3
    petersen.chromaticNumber is 3
    heawood.chromaticNumber is 2
    pappus.chromaticNumber is 2
    shrikhande.chromaticNumber is 4
    cubicalGraph.chromaticNumber is 2
    tetrahedralGraph.chromaticNumber is 4
    octahedralGraph.chromaticNumber is 3
    dodecahedralGraph.chromaticNumber is 3
    icosahedralGraph.chromaticNumber is 4
    completeBipartite(2, 3).chromaticNumber is 2
    completeBipartite(3, 3).chromaticNumber is 2
