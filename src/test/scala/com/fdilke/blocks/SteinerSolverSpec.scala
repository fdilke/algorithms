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
    solver(
      Reiterable.empty[Unit](()),
      0,
      _ => true
    ) is Some(Set.empty)
    solver(
      Reiterable.empty[Unit](()),
      1,
      _ => true
    ) is None
    
  test("one input, maybe one solution"):
    val theOne: Reiterable[Boolean] =
      Reiterable.one[Boolean](true, false)
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
