package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite
import GraphConstructions._

import scala.Seq
import scala.util.Random

class GraphConstructionsSpec extends FunSuite:
  
  test("torus(0, 0) is empty"):
    torus(0, 0).isEmpty is true

  test("torus(0, 3) is empty"):
    torus(0, 3).isEmpty is true

  test("torus(3, 0) is empty"):
    torus(3, 0).isEmpty is true

  test("torus(1, 1) has 1 vertex"):
    torus(1, 1) is Seq(0 -> 0)

  test("torus(3, 2) has 6 vertices and 12 edges"):
    val torus32: Seq[(Int, Int)] = torus(3, 2)
    torus32.flatMap:
      case (v: Int, w: Int) => Seq(v, w).toSet
    .distinct.sorted is (0 until 6)
    torus32.size is 12
    torus32.sorted is Seq(
      (0, 1), (0, 2),
      (1, 0), (1, 3),
      (2, 3), (2, 4),
      (3, 2), (3, 5),
      (4, 0), (4, 5),
      (5, 1), (5, 4)
    )

  // todo: add more adequate torus tests... but this is better than nothing
  test("The first odd graph is empty (should really be a singleton)"):
    oddGraph(1) is Seq.empty

  test("The second odd graph is a triangle"):
    oddGraph(2).sorted is Seq(
      (0, 1), (0, 2), (1, 2)
    )

  test("The second odd graph is Petersen"):
    oddGraph(3).sorted is Seq(
      (0, 7), (0, 8), (0, 9),
      (1, 5), (1, 6), (1, 9),
      (2, 4), (2, 6), (2, 8),
      (3, 4), (3, 5), (3, 7),
      (4, 9), (5, 8), (6, 7)
    )

  test("check a graph is antireflexive"):
    checkAntireflexive(Seq(
      Seq(false, true, false),
      Seq(true, false, true),
      Seq(false, true, false)
    ))
    intercept[IllegalArgumentException]:
      checkAntireflexive(Seq(
        Seq(true, false),
        Seq(true, false)
      ))

  test("check a graph is antireflexive"):
    checkSymmetric(Seq(
      Seq(false, true, false),
      Seq(true, false, true),
      Seq(false, true, false)
    ))
    intercept[IllegalArgumentException]:
      checkSymmetric(Seq(
        Seq(true, false),
        Seq(true, false)
      ))

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

  test("Can sort the vertices of a graph by descending degree"):
    val (order, inverse): (Seq[Int], Seq[Int]) =
      sortByDescDegree(Seq(
        Seq(false, true, false),
        Seq(true,  false, true),
        Seq(false, true, false)
      ))
    order.size is 3
    inverse.size is 3
    val diagonal: Seq[Int] = 0 until 3
    diagonal.map { i => order(inverse(i)) } is diagonal
    diagonal.map { i => inverse(order(i)) } is diagonal
    order.head is 1

  test("Can sort the vertices of a graph by descending degree (2)"):
    val (order, inverse): (Seq[Int], Seq[Int]) =
      sortByDescDegree(Seq(
        Seq(false, true, true, false, false),
        Seq(true, false, false, true, true),
        Seq(true, false, false, false, true),
        Seq(false, true, false, false, true),
        Seq(false, true, true, true, false)
      ))
    order.size is 5
    inverse.size is 5
    val diagonal: Seq[Int] = 0 until 3
    diagonal.map { i => order(inverse(i)) } is diagonal
    diagonal.map { i => inverse(order(i)) } is diagonal
    order.take(2).toSet is Set(1,4)

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
    val graph: Seq[Seq[Boolean]] =
      randomPlanar(size, Random(0L))
    graph.size is size
    (graph forall:
      _.size == size
    ) is true
    ColorGraphLoop(
      targetNumColors = 4,
      adjacencyTable = graph
    ).isDefined is true
