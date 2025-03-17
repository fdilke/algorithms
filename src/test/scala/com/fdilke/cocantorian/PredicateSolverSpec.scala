package com.fdilke.cocantorian

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*
import com.fdilke.utility.SetsUtilities.*

import java.util.concurrent.atomic.AtomicInteger

class ExPredicateSolverSpec extends PredicateSolverSpec(ExPredicateSolver):
  test("find a simple predicate"):
    solvesIn(7,
      2 -> true,
      3 -> false
    ): f =>
      f(2) & !f(3)


class PredicateSolverSpec(
  solver: PredicateSolver
)  extends FunSuite:
  def solvesIn(
    maxCalls: Int,
    entries: (Int, Boolean)*
  )(
    f: (Int => Boolean) => Boolean
  ): Unit =
    val numCalls = AtomicInteger()
    solver[Int]: int2bool =>
      numCalls.incrementAndGet()
      f(int2bool)
    .is:
      Some:
        entries.toMap
    println(s"numCalls = $numCalls")
    (numCalls.get <= maxCalls) is true

  test("solve for a trivial predicate"):
    solvesIn(1): f =>
      true

  test("detect an impossible equation"):
    solver[Int]: f=>
      f(2) == f(3) && f(3) == !f(4) && f(4) == f(2)
    is:
      None
