package com.fdilke.partition

import scala.annotation.targetName
import scala.math.Integral.Implicits._

object Partitions:
  // Return all sorted tuples of integers >= min > 0 which sum to total
  def apply(min: Int, total: Int): Seq[Seq[Int]] =
    if total == 0 then
      Seq(Seq())
    else if min == total then
      Seq(Seq(total))
    else if min > total then
      Seq()
    else
      val partitions: Seq[Seq[Seq[Int]]] =
        for
          m <- min to total
        yield apply(m, total - m) map:
            p => m +: p
      partitions.reduce:
        _ ++ _

  // Return all sorted tuples of integers >= 0 which sum to total
  def apply(n: Int): Seq[Seq[Int]] =
    apply(1, n)

  def count(min: Int, total: Int): Int =
    if total == 0 then
      1
    else if min == total then
      1
    else if min > total then
      0
    else
      val partitions: Seq[Int] =
        for
          m <- min to total
        yield
          count(m, total - m)
      partitions.sum

  def count(n: Int): Int =
    count(1, n)

  @targetName("nextVarargs")
  def next(partition: Int*): Option[Seq[Int]] =
    next(partition)

  def next(partition: Seq[Int]): Option[Seq[Int]] =
    partition match
      case Nil =>
        None
      case _ +: Nil =>
        None
      case first +: tail =>
        next(tail) match
          case Some(nextTail) =>
            Some(first +: nextTail)
          case None =>
            tail.headOption match
              case None =>
                throw IllegalArgumentException(s"shouldn't happen! partition = $partition")
              case Some(rest) =>
                if rest <= (first + 1) then
                  Some(Seq(first + rest))
                else
                  val (quotient, remainder) = (rest - 1) /% (first + 1)
                  if remainder == 0 then
                    Some(Seq.fill(quotient + 1)(first + 1))
                  else
                    Some(Seq.fill(quotient)(first + 1) :+ (remainder + first + 1))



