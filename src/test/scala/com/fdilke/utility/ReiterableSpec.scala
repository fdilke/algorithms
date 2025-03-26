package com.fdilke.utility

import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities.{bitCount, bits}
import munit.FunSuite

class ReiterableSpec extends FunSuite:
  test("a simple reiterable"):
    val simple: Reiterable[Int] =
      Reiterable[Int](
        initial = Some(0),
        nextOperator = n => Some(n + 1),
        continueCondition = _ < 7
      )
    println(s"simple = $simple")
    simple.toSeq is Seq(
      0, 1, 2, 3, 4, 5, 6
    )

  test("another simple reiterable"):
    val simple: Reiterable[Boolean] =
      Reiterable[Boolean](
        initial = Some(true),
        nextOperator = b => Some(!b),
        continueCondition = identity
      )
    simple.toSeq is Seq(
      true
    )

  test("a list reiterable"):
    Reiterable.list[Int](1,2).toSeq is
      Seq(1,2)

  test("the first element doesn't get a free pass"):
    Reiterable[Boolean](
      initial = Some(false),
      nextOperator = b => Some(!b),
      continueCondition = identity
    ).toSeq is
      Seq.empty
