package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.coloring.Graph._
import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

import scala.util.Random

class GraphSpec extends FunSuite:
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
    emptyGraph.isDistanceTransitive() is true
    Graph(
      false
    ).isDistanceTransitive() is true
    Graph(
      (0, 1), (1, 2), (2, 3), (3, 1)
    ).isDistanceTransitive() is false
    Graph(
      (0, 1)
    ) .isDistanceTransitive() is true
    Graph(
      (0, 1), (1, 2)
    ) .isDistanceTransitive() is false
    Graph(
      (0, 1), (1, 2), (2, 0)
    ) .isDistanceTransitive() is true
    petersen.isDistanceTransitive() is true
    heawood.isDistanceTransitive() is true
    pappus.isDistanceTransitive() is true
    cubicalGraph.isDistanceTransitive() is true
    dodecahedralGraph.isDistanceTransitive() is true
    completeBipartite(2, 3).isDistanceTransitive() is false
    completeBipartite(3, 3).isDistanceTransitive() is true
