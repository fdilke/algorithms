package com.fdilke.backtrack.node

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*

import java.util.concurrent.atomic.AtomicReference
import MonadIterable._
import NodeSolvers._
import com.fdilke.utility.Handy
import com.fdilke.utility.Handy.stackDepth

class NaiveNodeSolverSpec extends NodeSolverSpec(
  solver = NaiveNodeSolver,
  stackSafe = false
)

class StackSafeNodeSolverSpec extends NodeSolverSpec(
  solver = StackSafeNodeSolver,
  stackSafe = true
)

class StackSafeDedupNodeSolverSpec extends NodeSolverSpec(
  solver = StackSafeDedupNodeSolver,
  stackSafe = true
)

abstract class NodeSolverSpec(
  solver: NodeSolver,
  stackSafe: Boolean = true
) extends FunSuite:
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

  if stackSafe then
    test("is stack safe"):
      stackUse(10) is stackUse(5)
  else
    test("is unfortunately not stack safe"):
      (stackUse(10) > stackUse(5)) is true

  private def stackUse(maxRecursions: Int): Int =
    class DeepNode(recursions: Int = 0) extends NodeIterable[Int]:
      override def explore: NodeStatus =
        if recursions == maxRecursions then
          Iterable(solution(stackDepth()))
        else
          Iterable(node(DeepNode(recursions + 1)))
    solver.allSolutions(DeepNode()).head

class DupAndDedupSolversSpec extends FunSuite:
  // check dups and dedups for a deliberately redundant search for partitions of 7
  private inline val targetSum = 7
  private case class SumNode(
    values: Set[Int]
  ) extends NodeIterable[Set[Int]]:
    override def explore: NodeStatus =
      // println("exploring: " + values)
      val sum = values.sum
      if (sum == targetSum)
        Iterable(solution(values))
      else
        (1 to (targetSum - sum)) filterNot(values.contains) map: value =>
          node(SumNode(values + value))

  private val sumNode: SumNode = SumNode(Set())

  test("vanilla stacksafe solver enumerates solutions redundantly"):
    StackSafeNodeSolver.allSolutions(sumNode) is Seq(
      Set(1, 2, 4), Set(1, 4, 2), Set(1, 6), Set(2, 1, 4), Set(2, 4, 1), Set(2, 5),
      Set(3, 4), Set(4, 1, 2), Set(4, 2, 1), Set(4, 3), Set(5, 2), Set(6, 1), Set(7)
    )

  test("vanilla stacksafe solver enumerates solutions redundantly"):
    StackSafeDedupNodeSolver.allSolutions(sumNode) is Seq(
      Set(1, 2, 4), Set(1, 6), Set(2, 5), Set(3, 4), Set(7)
    )


