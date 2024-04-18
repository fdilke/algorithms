package com.fdilke.backtrack.node

import munit.FunSuite

import com.fdilke.utility.RichFunSuite._
import java.util.concurrent.atomic.AtomicReference

class SimpleNodeSolverSpec extends NodeSolverSpec(new SimpleNodeSearcher)

abstract class NodeSolverSpec(solver: NodeSolver) extends FunSuite:
  test("successfully fails at the first hurdle"):
    class NonStarterNode extends Node[Unit]:
      override def explore: NodeStatus =
        Left(Iterable.empty[Unit])
    solver.oneSolution(NonStarterNode()) is None

  test("successfully succeeds at the first hurdle"):
    class QuickWinNode extends Node[Int]:
      override def explore: NodeStatus =
        Left(Iterable[Int](2))
    val node = QuickWinNode()
    solver.oneSolution(node) is Some(2)

  test("successfully increments a value to 5"):
    class SearchNode(i: Int) extends Node[Boolean]:
      override def explore: NodeStatus =
        if (i == 5)
          Left(Iterable[Boolean](true))
        else
          Right(Iterable[Node[Boolean]](SearchNode(i + 1)))
    val initialNode = SearchNode(0)
    solver.oneSolution(initialNode) is Some(true)

  test("find a solution in a branching search"):
    val seqValues: Iterable[Boolean] = Iterable(true, false)
    val explorations: AtomicReference[Seq[Seq[Boolean]]] = AtomicReference[Seq[Seq[Boolean]]](Seq.empty)
    class SearchNode(prefix: Seq[Boolean]) extends Node[Seq[Boolean]]:
      override def explore: NodeStatus =
        explorations.set(explorations.get() :+ prefix)
        if (prefix.length == 3)
          Left(Iterable[Seq[Boolean]](prefix))
        else
          Right(seqValues.map { v => SearchNode(prefix :+ v) })
    val initialNode = SearchNode(Seq.empty)
    solver.oneSolution(initialNode) is Some(
      Seq(true, true, true)
    )
    explorations.get().size is 4

  test("find all solutions in a branching search"):
    val seqValues: Iterable[Boolean] = Iterable(true, false)
    class SearchNode(prefix: Seq[Boolean]) extends Node[Seq[Boolean]]:
      override def explore: NodeStatus =
        if (prefix.length == 3)
          Left(Iterable[Seq[Boolean]](prefix))
        else
          Right(seqValues.map { v => SearchNode(prefix :+ v) })
    checkSameElementsAs(  
      solver.allSolutions(SearchNode(Seq.empty)).toSeq,
      Seq(
        Seq(true, true, true),
        Seq(true, true, false),
        Seq(true, false, true),
        Seq(true, false, false),
        Seq(false, true, true),
        Seq(false, true, false),
        Seq(false, false, true),
        Seq(false, false, false)
      )
    )
