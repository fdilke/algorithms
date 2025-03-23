package com.fdilke.utility

import scala.collection.mutable
import scala.runtime.Arrays
import scala.math.BigInt

object RisingInts:
  def apply(rising: Seq[Int]): RisingInts =
    RisingInts(rising.toArray)
  def fromBits(bits: Int): RisingInts =
    fromBits(BigInt(bits))
  def fromBits(bits: BigInt): RisingInts =
    if (bits.signum == -1)
      throw new IllegalArgumentException("negative bit pattern")
    val set: mutable.Buffer[Int] =
      mutable.Buffer[Int]()
    for
      i <- 0 until bits.bitLength
    do
      if bits.testBit(i) then
        set += i
    RisingInts(set.toArray)

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

  def toBits: BigInt =
    array.foldLeft(BigInt(0)): (bigint, i) =>
      bigint.setBit(i)

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

