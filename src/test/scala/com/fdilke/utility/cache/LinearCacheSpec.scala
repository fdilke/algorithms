package com.fdilke.utility.cache

import com.fdilke.utility.RichFunSuite.*
import com.fdilke.utility.cache.TriangularCache
import munit.FunSuite

import scala.collection.mutable

class LinearCacheSpec extends FunSuite:
  class LocalScope:
    val calls: mutable.ListBuffer[Int] =
      mutable.ListBuffer[Int]()

    def fn(n: Int): String =
      calls.append(n)
      s"$n"

  private val fixture: FunFixture[LocalScope] =
    FunFixture[LocalScope](
      setup = test => new LocalScope,
      teardown = { scope => () }
    )

  fixture.test("memoized vanilla function acts as pass through,is cached"): scope =>
    val cache: Int => String =
      LinearCache(scope.fn)

    cache(1) is "1"
    scope.calls is Seq(1)
    cache(3) is "3"
    scope.calls is Seq(1, 3)
    cache(3) is "3"
    scope.calls is Seq(1, 3)
    cache(8) is "8"
    scope.calls is Seq(1, 3, 8)
