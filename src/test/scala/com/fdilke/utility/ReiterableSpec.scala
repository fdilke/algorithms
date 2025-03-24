package com.fdilke.utility

import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities.{bitCount, bits}
import munit.FunSuite

class ReiterableSpec extends FunSuite:
  test("a simple reiterable"):
    val simple: Iterable[Int] =
      ReiterableLite2[Int](
        initial = 0,
        nextOperator = _ + 1,
        continuePredicate = _ < 7
      )
    println(s"simple = $simple")
    simple.toSeq is Seq(
      0, 1, 2, 3, 4, 5, 6
    )

  test("another simple reiterable"):
    val simple: Iterable[Boolean] =
      ReiterableLite2[Boolean](
        initial = true,
        nextOperator = !_,
        continuePredicate = x => x
      )
    simple.toSeq is Seq(
      true
    )
