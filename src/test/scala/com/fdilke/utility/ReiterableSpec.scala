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

  test("clone the state as a reiterator"):
    val iterator: Reiterator[Int] =
      Reiterable.list(1,2,3).iterator
    iterator.hasNext is true
    iterator.next() is 1
    val dupIterator: Reiterator[Int] =
      iterator.duplicateState()
    iterator.hasNext is true
    iterator.next() is 2
    iterator.hasNext is true
    iterator.next() is 3
    iterator.hasNext is false
    dupIterator.hasNext is true
    dupIterator.next() is 2
    dupIterator.hasNext is true
    dupIterator.next() is 3
    dupIterator.hasNext is false
