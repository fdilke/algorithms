package com.fdilke.backtrack.node

import munit.FunSuite

import com.fdilke.utility.RichFunSuite._
import java.util.concurrent.atomic.AtomicReference
import MonadIterable._

class SimpleNodeSolverSpec extends NodeSolverSpec(new SimpleNodeSearcher)

abstract class NodeSolverSpec(solver: NodeSolver) extends FunSuite:
  test("successfully fails at the first hurdle"):
    class NonStarterNode extends NodeIterable[Unit]:
      override def explore: NodeStatus =
        Iterable.empty
    solver.allSolutions(NonStarterNode()).headOption is None

  test("successfully succeeds at the first hurdle"):
    class QuickWinNode extends NodeIterable[Int]:
      override def explore: NodeStatus =
        Iterable(solution(2))
    val node = QuickWinNode()
    solver.allSolutions(node).headOption is Some(2)

  test("successfully increments a value to 5"):
    class SearchNode(i: Int) extends NodeIterable[Boolean]:
      override def explore: NodeStatus =
        if (i == 5)
          Iterable(solution(true))
        else
          Iterable(node(SearchNode(i + 1)))
    val initialNode = SearchNode(0)
    solver.allSolutions(initialNode).headOption is Some(true)

  test("find a solution in a branching search"):
    val seqValues: Iterable[Boolean] =
      Iterable(true, false)
    val explorations: AtomicReference[Seq[Seq[Boolean]]] =
      AtomicReference[Seq[Seq[Boolean]]](Seq.empty)
    class SearchNode(prefix: Seq[Boolean]) extends NodeIterable[Seq[Boolean]]:
      override def explore: NodeStatus =
        explorations.set(explorations.get() :+ prefix)
        if (prefix.length == 3)
          Iterable(solution(prefix))
        else
          seqValues.map { v => node(SearchNode(prefix :+ v)) }
    val initialNode = SearchNode(Seq.empty)
    solver.allSolutions(initialNode).headOption is Some(
      Seq(true, true, true)
    )
    explorations.get().size is 15

  test("find all solutions in a branching search"):
    val seqValues: Iterable[Boolean] = Iterable(true, false)
    class SearchNode(prefix: Seq[Boolean]) extends NodeIterable[Seq[Boolean]]:
      override def explore: NodeStatus =
        if (prefix.length == 3)
          Iterable(solution(prefix))
        else
          seqValues.map { v => node(SearchNode(prefix :+ v)) }
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

// do this in an IO?
//  test("break the stack"):
//    val maxDepth = 2000
//    class DeepNode(depth: Int) extends NodeIterable[Boolean]:
//      override def explore: NodeStatus =
//        if (depth == maxDepth)
//          Iterable(solution(true))
//        else
//          Iterable(node(DeepNode(depth + 1)))
//    val error = intercept[StackOverflowError]:
//      checkSameElementsAs(
//        solver.allSolutions(DeepNode(0)).toSeq,
//        Seq(true)
//      )
//    println("Error = " + error)
