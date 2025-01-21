package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite
import GraphConstructions.*

import scala.Seq

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


