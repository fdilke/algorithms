package com.fdilke.blocks

import com.fdilke.backtrack.node.coloring.Graph
import com.fdilke.utility.Reiterable
import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class GreedySteinerSolverSpec extends
  SteinerSolverSpec(GreedySteinerSolver)
class PortionControlledSteinerSolverSpec extends
  SteinerSolverSpec(PortionControlledSteinerSolver)

class SteinerSolverSpec(
  solver: SteinerSolver
) extends FunSuite:
  test("no inputs, no solution"):
    val theEmpty: Reiterable[Unit] =
      Reiterable.list[Unit]()
    solver(
      theEmpty,
      0,
      _ => true
    ) is Some(Set.empty)
    solver(
      theEmpty,
      1,
      _ => true
    ) is None

  test("one input, maybe one solution"):
    val theOne: Reiterable[Boolean] =
      Reiterable.list[Boolean](true)
    solver(
      theOne,
      0,
      _ => true
    ) is Some(Set.empty)
    solver(
      theOne,
      1,
      _ => true
    ) is Some(Set(true))
    solver(
      theOne,
      2,
      _ => true
    ) is None

  test("find a complete subgraph"):
    val graph: Graph =
      Graph(
        (0, 2), (1, 2), (2, 3), (3, 0)
      )
    val nodes: Reiterable[Int] =
      Reiterable.list(0, 1, 2, 3)
    solver(
      nodes,
      3,
      (v, w) => graph.adjacencyTable(v)(w)
    ) is Some(
      Set(0, 2, 3)
    )