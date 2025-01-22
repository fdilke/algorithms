package com.fdilke.backtrack.node.coloring

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*

import scala.annotation.targetName
import GraphConstructions.{oddGraph, torus}
import cats.Monad
import com.fdilke.backtrack.node.NodeSolvers.StackSafeDedupNodeSolver
import com.fdilke.backtrack.node.coloring.ColorGraph.{adjacencyTableFromPairs, lastVertexFromPairs}
import com.fdilke.backtrack.node.coloring.NodeLazyFail.node
import com.fdilke.backtrack.node.MonadIterable.*
import com.fdilke.backtrack.node.Node

class ColorGraphSpec extends FunSuite:
  
  private def checkColoring(
    numColors: Int,
    checkMinimal: Boolean,
    coloring: Option[Seq[Int]],
    adjacencyTable: Seq[Seq[Boolean]],
  ): Unit =
    coloring match
      case None => fail("coloring not found")
      case Some(coloring) =>
        for
          i <- coloring.indices
          j <- 0 until i
        do
          if adjacencyTable(i)(j) && (coloring(i) == coloring(j)) then
            fail("adjacent vertices have same color")
        if coloring.distinct.size > numColors then
          fail("too many colors")
        if checkMinimal && PartialColoring.fromColorsAndAdjacencies(
          coloring,
          adjacencyTable
        ).amalgamations.nonEmpty then
          fail("not a minimal coloring: an amalgamation is possible")

  //noinspection AccessorLikeMethodIsUnit
  private def canColor(
    numColors: Int,
    checkMinimal: Boolean,
    adjacencyTable: Seq[Seq[Boolean]]
  ): Unit =
    if numColors > 0 && ColorGraph(numColors - 1, adjacencyTable).isDefined then
       fail("this many colors are not required")
    checkColoring(
      numColors,
      checkMinimal,
      ColorGraph(numColors, adjacencyTable),
      adjacencyTable
    )

  //noinspection AccessorLikeMethodIsUnit
  @targetName("canJustColorWithUnpackedAdjacencyTable")
  private def canColor(
    numColors: Int,
    checkMinimal: Boolean,
    unpackedAdjacencyTable: Boolean*
  ): Unit =
    canColor(numColors, checkMinimal, ColorGraph.packAdjacencyTable(unpackedAdjacencyTable))

  //noinspection AccessorLikeMethodIsUnit
  @targetName("canJustColorWithAdjacencyPairs")
  private def canColor(
    numColors: Int,
    checkMinimal: Boolean,
    adjacencyPairs: (Int, Int)*
  ): Unit =
    canColor(numColors, checkMinimal, ColorGraph.adjacencyTableFromPairs(adjacencyPairs*))

  test("Can compute the last vertex from pairs"):
    ColorGraph.lastVertexFromPairs() is -1
    ColorGraph.lastVertexFromPairs(0 -> 1) is 1
    ColorGraph.lastVertexFromPairs(7 -> 8, 0 -> 1) is 8
    ColorGraph.lastVertexFromPairs(2 -> 3, 4 -> 0, 5 -> 6, 0 -> 1) is 6

  test("Can construct an adjacency table from pairs"):
    ColorGraph.adjacencyTableFromPairs() is Seq()
    ColorGraph.adjacencyTableFromPairs((0, 1)) is Seq(Seq(false, true), Seq(true, false))
    ColorGraph.adjacencyTableFromPairs((1, 2)) is Seq(
      Seq(false, false, false),
      Seq(false, false, true),
      Seq(false, true, false)
    )
    ColorGraph.adjacencyTableFromPairs((0, 1), (1, 2)) is Seq(
      Seq(false, true, false),
      Seq(true,  false, true),
      Seq(false, true, false)
    )
    ColorGraph.adjacencyTableFromPairs((0, 3), (1, 2)) is Seq(
      Seq(false, false, false, true),
      Seq(false, false, true, false),
      Seq(false, true, false, false),
      Seq(true,  false, false, false)
    )

  test("Reject graphs unless they're antireflexive & symmetric"):
    intercept[IllegalArgumentException]:
      ColorGraph(2, 1 -> 1)
    .getMessage is
      "adjacency table must be antireflexive: fail at 1"

    intercept[IllegalArgumentException]:
      ColorGraph(2,
        false, false,
        true, false
      )
    .getMessage is
      "adjacency table must be symmetric: fail at 0, 1"

  test("Can color the empty graph (with 0 colors)"):
    canColor(0, true, Seq[Seq[Boolean]]())

  test("Can color the empty graph (with 0 colors) by adjacencies"):
    canColor(0, true, Seq.empty[Boolean]*)

  test("Can color a trivial graph with 1 vertex"):
    canColor(1, true, Seq(Seq(false)))

  test("Can color a disconnected graph with 2 vertexes"):
    canColor(1, true, Seq(Seq(false, false), Seq(false, false)))
    
  test("Can color a graph with 2 joined vertexes"):
    canColor(2, true, Seq(Seq(false, true), Seq(true, false)))
    
  test("Can color a graph with 2 joined vertexes, using an unpacked adjacency table"):
    canColor(2, true, false, true, true, false)

  test("Can color a graph with 2 joined vertexes by adjacencies"):
    canColor(2, true, 0 -> 1)

  test("chi(torus(2,2)) == 3"):
    canColor(2, true, torus(2, 2)*)

  test("chi(torus(2,3)) == 3"):
    canColor(3, true, torus(2, 3)*)

  test("chi(torus(3,2)) == 3"):
    canColor(3, true, torus(3, 2)*)

  test("chi(torus(3,3)) == 3"):
    canColor(3, true, torus(3, 3)*)

// too slow: 3.2 sec
//  test("chi(Petersen) == 3"):
//    val petersen = oddGraph(3)
//    canColor(3, true, petersen*)

// too slow - ~5sec */
    
//  test("chi(torus(5,2)) <= 3"):
//    canColor(3, false, torus(5, 2)*)
//
//  test("chi(torus(2,5)) <= 3"):
//    canColor(3, false, torus(2, 5)*)

/* MUCH too slow :( */
    
//  test("chi(torus(3,4)) <= 3"):
//    canColor(3, false, torus(3, 4)*)

//
//  test("torus(4, 3) requires 3 colors"):
//    canJustColor(3, torus(4, 3)*)
//
//  test("torus(4, 4) requires 2 colors"):
//    canJustColor(3, torus(4, 3)*)

  // Making sure the iteration works as it should - currently doesn't
  test("can quickly color a graph with enough colors".ignore):
    canColor(25, false, torus(5, 5)*)
//    canColor(100, false, torus(10, 10)*)


object NodeLazyFail extends App:
  def lazyExplode[
    NODE <: Node[NODE, Iterable, SOLUTION],
    SOLUTION
  ](
    node: NODE
  ): Iterable[SOLUTION] =
    node.explore.flatMap:
      case Left(node2) => lazyExplode(node2)
      case Right(solution) => Iterable(solution)
      
//    Iterable.unfold()
//  Monad[Iterable].tailRecM[PartialColoring, Seq[Int]](node):
//    _.explore

  val adjacencyPairs: Seq[(Int, Int)] = torus(1, 11) // torus(5, 5)
  val adjacencyTable: Seq[Seq[Boolean]] = adjacencyTableFromPairs(adjacencyPairs*)
  val lastVertex = lastVertexFromPairs(adjacencyPairs*)
  // StackSafeDedupNodeSolver.allSolutions[PartialColoring, Iterable, Seq[Int]]:
  val node: PartialColoring =
    PartialColoring(
      0 to lastVertex,
      adjacencyTable
    )
  println("Exploring...")
  val exp: Iterable[Either[PartialColoring, Seq[Int]]] =
    node.explore
  println("Taking the head...")
  val h: Either[PartialColoring, Seq[Int]] = exp.head
  h match
    case Left(coloring) =>
      println("coloring: " + coloring)
    case Right(solution) =>
      println("solution: " + solution)
  println("Exploding...")
  val exploded: Iterable[Seq[Int]] =
    lazyExplode(node)
  println("Exploding...done")
