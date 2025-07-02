package com.fdilke.partition

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite
import scala.math.Ordering.Implicits._

class PartitionsSpec extends FunSuite:
  test("can enumerate partitions with a specified minimum"):
    Partitions(1, 0) is Seq(Seq())
    Partitions(2, 0) is Seq(Seq())
    Partitions(2, 1) is Seq()
    Partitions(1, 1) is Seq(Seq(1))
    Partitions(1, 2) is Seq(Seq(1, 1), Seq(2))

  test("can enumerate partitions"):
    Partitions(0) is Seq(Seq())
    Partitions(1) is Seq(Seq(1))
    Partitions(2) is Seq(Seq(1, 1), Seq(2))
    Partitions(3) is Seq(Seq(1, 1, 1), Seq(1, 2), Seq(3))
    Partitions(4) is Seq(
      Seq(1, 1, 1, 1),
      Seq(1, 1, 2),
      Seq(1, 3),
      Seq(2, 2),
      Seq(4)
    )
    Partitions(5) is Seq(
      Seq(1, 1, 1, 1, 1),
      Seq(1, 1, 1, 2),
      Seq(1, 1, 3),
      Seq(1, 2, 2),
      Seq(1, 4),
      Seq(2, 3),
      Seq(5)
    )
    Partitions(6) is Seq(
      Seq(1, 1, 1, 1, 1, 1),
      Seq(1, 1, 1, 1, 2),
      Seq(1, 1, 1, 3),
      Seq(1, 1, 2, 2),
      Seq(1, 1, 4),
      Seq(1, 2, 3),
      Seq(1, 5),
      Seq(2, 2, 2),
      Seq(2, 4),
      Seq(3, 3),
      Seq(6)
    )
    Partitions(7) is Seq(
      Seq(1, 1, 1, 1, 1, 1, 1),
      Seq(1, 1, 1, 1, 1, 2),
      Seq(1, 1, 1, 1, 3),
      Seq(1, 1, 1, 2, 2),
      Seq(1, 1, 1, 4),
      Seq(1, 1, 2, 3),
      Seq(1, 1, 5),
      Seq(1, 2, 2, 2),
      Seq(1, 2, 4),
      Seq(1, 3, 3),
      Seq(1, 6),
      Seq(2, 2, 3),
      Seq(2, 5),
      Seq(3, 4),
      Seq(7)
    )

  test("partitions are always ordered lexicographically"):
    for
      n <- 0 to 30
    do
      val p = Partitions(n)
      for
        i <- 0 until (p.size - 1)
      do
        (p(i) < p(i + 1)) is true

  test("can count partitions"):
    for
      n <- 0 to 20
    do
      Partitions(n).size.toLong is Partitions.count(n)

  test("can compute the 'next' partition"):
    Partitions.next() is None
    Partitions.next(1) is None
    Partitions.next(1, 1) is Some(Seq(2))
    Partitions.next(1, 4) is Some(Seq(2, 3))
    Partitions.next(1, 1, 2, 3) is Some(Seq(1, 1, 5))
    Partitions.next(1, 3, 3) is Some(Seq(1, 6))
    Partitions.next(7) is None
    for
      n <- 0 to 20
    do
      val p = Partitions(n)
      for
        i <- 0 until (p.size - 1)
      do
        Partitions.next(p(i)) is Some(p(i + 1))
