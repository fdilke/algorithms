package com.fdilke.blocks

import com.fdilke.utility.SetsUtilities._
import scala.collection.mutable

object DivisibilityConditions:
  def apply(
    lambda: Int,
    r: Int,
    q: Int,
    n: Int
  ): Boolean =
    (0 until r) forall: i =>
      localCondition(lambda, r, q, n, i)

  private inline def localCondition(
    lambda: Int,
    r: Int,
    q: Int,
    n: Int,
    i: Int
  ): Boolean =
      (lambda * nCrB(n - i, r - i)) % nCrB(q - i, r - i) == BigInt(0)
  
  def onN(
    lambda: Int,
    r: Int,
    q: Int,
  ): (Int, Seq[Int]) =
    val ncrb: Seq[BigInt] =
      (0 until r).map: i =>
        nCrB(q - i, r - i)
    val mainLcmB: BigInt = 
      ncrb.foldLeft(BigInt(1))(lcm)
    if mainLcmB > Int.MaxValue then
      throw new IllegalArgumentException("lcm exceeds Int maximum")
    val mainLcm: Int =
      mainLcmB.toInt
    val globalResiduesAllowed: mutable.Seq[Boolean] =
      mutable.Seq.fill(mainLcm):
        true
    for 
      i <- 0 until r
    do
      val localResiduesAllowed: Array[Boolean] =
        (0 until ncrb(i).toInt).map: n =>
          localCondition(lambda, r, q, n, i)
        .toArray
      for
        n <- 0 until mainLcm
      do 
        if !localResiduesAllowed(n % ncrb(i).toInt) then
          globalResiduesAllowed(n) = false
    mainLcm -> (0 until mainLcm).filter: n =>
      globalResiduesAllowed(n)

  def leastN(
    lambda: Int,
    r: Int,
    q: Int
  ):Int =
    LazyList.iterate(q + 1)(_ + 1).find: n =>
      DivisibilityConditions(lambda = 1, r = r, q = q, n = n)
    .get
    