package com.fdilke.blocks

import com.fdilke.utility.Reiterable
import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class GreedySteinerSolverSpec extends
  SteinerSolverSpec(GreedySteinerSolver)

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
