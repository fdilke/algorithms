package com.fdilke.backtrack

import com.fdilke.backtrack.node.MonadIterable
import com.fdilke.utility.Handy.stackDepth
import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

import java.util.concurrent.atomic.AtomicReference
import scala.language.postfixOps

class BacktrackIterableSpec extends GenericBacktrackIterableSolverSpec(
  solver = BacktrackIterable
)

class BacktrackIterableDedupSpec extends GenericBacktrackIterableSolverSpec(
  solver = BacktrackIterable.dedup,
)

class BacktrackIterableDedupLegacySpec extends GenericBacktrackIterableSolverSpec(
  solver = BacktrackIterable.dedupLegacy,
)

abstract class GenericBacktrackIterableSolverSpec(
  solver: BacktrackIterableSolver
) extends FunSuite:
  test("successfully fails at the first hurdle"):
    class NonStarterNode
    solver(NonStarterNode()): _ =>
      Iterable.empty
    .headOption is None

  test("successfully succeeds at the first hurdle"):
    class QuickWinNode
    solver(QuickWinNode()): _ =>
      Iterable(Right(2))
    .headOption is Some(2)

  test("successfully increments a value to 5"):
    solver[Int, Boolean](0): i =>
      if (i == 5)
        Iterable(Right(true))
      else
        Iterable(Left(i + 1))
    .headOption is Some(true)

  test("find a solution in a branching search"):
    val seqValues: Iterable[Boolean] =
      Iterable(true, false)
    val explorations: AtomicReference[Seq[Seq[Boolean]]] =
      AtomicReference[Seq[Seq[Boolean]]](Seq.empty)
    solver(Seq.empty[Boolean]): prefix =>
        explorations.set(explorations.get() :+ prefix)
        if (prefix.length == 3)
          Iterable(Right(prefix))
        else
          seqValues.map: v =>
            Left(prefix :+ v)
    .toSet is Set(
      Seq(false, false, false),
      Seq(false, false, true),
      Seq(false, true, false),
      Seq(false, true, true),
      Seq(true, false, false),
      Seq(true, false, true),
      Seq(true, true, false),
      Seq(true, true, true)
    )
    explorations.get().size is 15

  test("find all solutions in a branching search (2)"):
    val seqValues: Iterable[Boolean] = Iterable(true, false)
    solver[Seq[Boolean], Seq[Boolean]](
      Seq.empty[Boolean]
    ): prefix =>
      if (prefix.length == 3)
        Iterable(Right(prefix))
      else
        seqValues.map: v =>
          Left(prefix :+ v)
    .isSet:
      Set(
        Seq(true, true, true),
        Seq(true, true, false),
        Seq(true, false, true),
        Seq(true, false, false),
        Seq(false, true, true),
        Seq(false, true, false),
        Seq(false, false, true),
        Seq(false, false, false)
      )

  test("is stack safe"):
    def stackUse(maxRecursions: Int): Int =
      solver[Int, Int](0): recursions =>
        Iterable:
          if recursions == maxRecursions then
            Right(stackDepth())
          else
            Left(recursions + 1)
      .head
    stackUse(10) is stackUse(5)


class DupAndDedupBacktrackIterableSpec extends FunSuite:
  // check dups and dedups for a deliberately redundant search for partitions of 7
  private inline val targetSum = 7
  private case class SumNode(
    values: Set[Int]
  )
  private def explore(
    node: SumNode
  ): Iterable[Either[SumNode, Set[Int]]] =
      val sum = node.values.sum
      if (sum == targetSum)
        Iterable(Right(node.values))
      else
        (1 to (targetSum - sum)) filterNot node.values.contains map: value =>
          Left(SumNode(node.values + value))

  private val sumNode: SumNode = SumNode(Set())

  test("vanilla backtracker enumerates solutions redundantly"):
    BacktrackIterable(sumNode)(explore).toSet is Set(
      Set(1, 2, 4), Set(1, 4, 2), Set(1, 6), Set(2, 1, 4), Set(2, 4, 1), Set(2, 5),
      Set(3, 4), Set(4, 1, 2), Set(4, 2, 1), Set(4, 3), Set(5, 2), Set(6, 1), Set(7)
    )

  test("dedup backtracker enumerates solutions irredundantly"):
    BacktrackIterable.dedup(sumNode)(explore).toSet is Set(
      Set(1, 2, 4), Set(1, 6), Set(2, 5), Set(3, 4), Set(7)
    )
