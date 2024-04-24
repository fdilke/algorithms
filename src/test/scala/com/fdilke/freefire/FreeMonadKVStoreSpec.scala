package com.fdilke.freefire

import com.fdilke.backtrack.Backtrack._
import munit.FunSuite
import FreeMonadKVStore._
import com.fdilke.utility.RichFunSuite._

class FreeMonadKVStoreSpec extends FunSuite:
  test("run a program impurely"):
    val prog = littleProgram()
    impureRun(prog) is Some(14)


