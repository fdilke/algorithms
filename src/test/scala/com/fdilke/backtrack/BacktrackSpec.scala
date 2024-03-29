package com.fdilke.backtrack

import com.fdilke.backtrack.Backtrack._
import munit.FunSuite

class BacktrackSpec extends FunSuite:
  test("enumerates the void"):
    lazy val nodeVoid: DecisionNode[Int, Boolean] =
      _ => MapInvalid.asInstanceOf[NextStep[Int, Boolean]]

    assertEquals(
      Backtrack.solve[Int, Boolean](
        Iterable(true, false),
        nodeVoid
      ).toSet,
      Set.empty
    )

  test("enumerates all injections { 0, 1 } -> Boolean"):
    lazy val nodeInjections01Bool: DecisionNode[Int, Boolean] =
      map => map.size match {
        case 0 => MapContinue(0, nodeInjections01Bool)
        case 1 => MapContinue(1, nodeInjections01Bool)
        case 2 =>
          if map(0) != map(1) then
            MapComplete.asInstanceOf[NextStep[Int, Boolean]]
          else
            MapInvalid.asInstanceOf[NextStep[Int, Boolean]]
      }

    assertEquals(
      Backtrack.solve[Int, Boolean](
        Iterable(true, false),
        nodeInjections01Bool
      ).toSet,
      Set[Map[Int, Boolean]](
        Map( 0 -> true, 1 -> false ),
        Map( 1 -> true, 0 -> false )
      )
    )
