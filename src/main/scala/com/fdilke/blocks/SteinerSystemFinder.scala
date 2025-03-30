package com.fdilke.blocks

import com.fdilke.utility.{Reiterable, SetsUtilities}
import com.fdilke.utility.SetsUtilities.nCrB

object SteinerSystemFinder:
  def apply(
    r: Int,
    q: Int,
    n: Int
  ): Option[SteinerSystem] =
    val enumeration: Reiterable[RisingInts] =
      RisingInts.qSubsetsOfN(q=q, n=n)
    val numBlocks =
      (nCrB(n, r) / nCrB(q, r)).toInt
    PortionControlledSteinerSolver[RisingInts](
      inputs = enumeration,
      solutionSize = numBlocks,
      compatible = (t: RisingInts, u: RisingInts) =>
        (t intersect u).size < r
    ) map:
      blocks =>
        SteinerSystem(r, q, n, blocks)

case class SteinerSystem(
  r: Int,
  q: Int,
  n: Int,
  blocks: Set[RisingInts]
):
  def sanityTest(): Unit =
    val expectedSize: Int =
      (nCrB(n, r) / nCrB(q, r)).toInt
    if blocks.size != expectedSize then
      throw new IllegalArgumentException(s"${blocks.size} blocks, expected $expectedSize")
    else if !blocks.forall:
      _.size == q
    then
      throw new IllegalArgumentException(s"blocks should have size $q")
    else if !blocks.forall:
      _.array.last < n
    then
      throw new IllegalArgumentException(s"block values should be < $n")
    else if !RisingInts.qSubsetsOfN(r, n).forall: rSet =>
      blocks.count:
        _ contains rSet
      == 1
    then
      throw new IllegalArgumentException(s"every r-set should be in a unique block")
