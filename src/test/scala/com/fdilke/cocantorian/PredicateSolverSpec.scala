package com.fdilke.cocantorian

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._

class PredicateSolverSpec  extends FunSuite:
  test("find a simple predicate"):
    PredicateSolver[Int]:
      f => f(2) && !f(3)
    match
      case None => fail("should find solution")
      case Some(map) =>
        map is Map[Int, Boolean](
          2 -> true,
          3 -> false
        )

  test("detect an impossible predicate equation"):
    PredicateSolver[Int]: f=>
      f(2) == f(3) && f(3) == !f(4) && f(4) == f(2)
    .is(None)
