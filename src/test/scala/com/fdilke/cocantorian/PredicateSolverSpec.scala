package com.fdilke.cocantorian

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._

class ExPredicateSolverSpec extends PredicateSolverSpec(ExPredicateSolver)

class PredicateSolverSpec(
  solver: PredicateSolver
)  extends FunSuite:
  test("find a simple predicate"):
    solver[Int]:
      f => f(2) && !f(3)
    is:
      Some:
        Map[Int, Boolean](
          2 -> true,
          3 -> false
        )
    
  test("detect an impossible predicate equation"):
    solver[Int]: f=>
      f(2) == f(3) && f(3) == !f(4) && f(4) == f(2)
    .is(None)
