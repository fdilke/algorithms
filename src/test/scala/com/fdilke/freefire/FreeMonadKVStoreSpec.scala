package com.fdilke.freefire

import com.fdilke.backtrack.MapBacktrack._
import munit.FunSuite
import FreeMonadKVStore._
import com.fdilke.utility.RichFunSuite._
import cats.data.State

class FreeMonadKVStoreSpec extends FunSuite:
  test("run a program impurely"):
    val prog = littleProgram()
    impureRun(prog) is Some(14)

  test("run a program purely"):
    val prog = littleProgram()
    val state: State[Map[String, Any], Option[Int]] = pureRun(prog)
    val (map: Map[String, Any], result: Option[Int]) = state.run(Map.empty[String, Any]).value
    map is Map[String, Any]("wild-cats" -> 14)
    result is Some(14)


