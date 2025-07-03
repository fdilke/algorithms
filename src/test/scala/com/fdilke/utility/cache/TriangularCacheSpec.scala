package com.fdilke.utility.cache

import com.fdilke.utility.RichFunSuite.*
import com.fdilke.utility.cache.TriangularCache
import munit.FunSuite

import scala.collection.mutable

class TriangularCacheSpec extends FunSuite:
  class LocalScope:
    val calls: mutable.ListBuffer[(Int, Int)] =
      mutable.ListBuffer[(Int, Int)]()

    def fn(m: Int, n: Int): String =
      calls.append((m, n))
      s"$m;$n"

  private val fixture: FunFixture[LocalScope] =
    FunFixture[LocalScope](
      setup = test => new LocalScope,
      teardown = { scope => () }
    )

  fixture.test("memoized vanilla function acts as pass through,is cached"): scope =>
    val cache: (Int, Int) => String =
      TriangularCache(scope.fn)

    cache(1, 2) is "1;2"
    scope.calls is Seq((1, 2))
    cache(2, 3) is "2;3"
    scope.calls is Seq((1, 2), (2, 3))
    cache(1, 2) is "1;2"
    scope.calls is Seq((1, 2), (2, 3))
    cache(7, 8) is "7;8"
    scope.calls is Seq((1, 2), (2, 3), (7, 8))
