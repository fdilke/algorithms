package com.fdilke.partition

import com.fdilke.utility.Memoize

import scala.annotation.targetName
import scala.math.Integral.Implicits.*

object Partitions:
  // Return all sorted tuples of integers >= min > 0 which sum to total
  private val memoizedApply: (Int, Int) => Seq[Seq[Int]] =
    Memoize:
      (min: Int, total: Int) =>
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
            yield memoizedApply(m, total - m) map:
                p => m +: p
          partitions.reduce:
            _ ++ _

  def apply(min: Int, total: Int): Seq[Seq[Int]] =
    memoizedApply(min, total)

  // Return all sorted tuples of integers >= 0 which sum to total
  def apply(n: Int): Seq[Seq[Int]] =
    apply(1, n)

  private val bigInt0 = BigInt(0)
  private val bigInt1 = BigInt(1)
  
  private val memoizedCount: (Int, Int) => BigInt =
    Memoize:
      (min: Int, total: Int) =>
        if total == 0 then
          bigInt1
        else if min == total then
          bigInt1
        else if min > total then
          bigInt0
        else
          val partitions: Seq[BigInt] =
            for
              m <- min to total
            yield
              memoizedCount(m, total - m)
          partitions.sum

  def count(min: Int, total: Int): BigInt =
    memoizedCount(min, total)

  def count(n: Int): BigInt =
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



