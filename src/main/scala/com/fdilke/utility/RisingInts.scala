package com.fdilke.utility

import scala.runtime.Arrays

object RisingInts:
  def apply(rising: Seq[Int]): RisingInts =
    RisingInts(rising.toArray)

case class RisingInts(
  array: Array[Int]
):
  def sanityTest(): Unit =
    for
      i <- 0 until array.length - 1
    do
      if array(i) >= array(i+1) then
        throw new IllegalArgumentException("array is not monotonically increasing")
    if !array.forall:
      _ >= 0
    then
      throw new IllegalArgumentException("values must be nonnegative")

  override def equals(obj: Any): Boolean =
    obj match
      case RisingInts(other) =>
        array.sameElements(other.array)
      case _ => false

  def next: RisingInts =
    if (array.length == 0)
      throw new IllegalArgumentException("cannot apply to empty sequence")
    else
      val head: Int =
        array(0)
      val seq: Seq[Int] =
        array.zipWithIndex.find: (v, i) =>
          v != head + i
        match
          case None =>
            val a = head
            val b = array.length
            (0 until (b - 1)) :+ (head + b)
          case Some((endBlock, i)) =>
            (0 until (i - 1)) ++ (( head + i ) +: array.toSeq.drop(i))
      RisingInts(seq.toArray)

