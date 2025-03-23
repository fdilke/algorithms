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
      (lambda * nCr(n - i, r - i)) % nCr(q - i, r - i) == 0
  
  def onN(
    lambda: Int,
    r: Int,
    q: Int,
  ): (Int, Seq[Int]) =
    val ncr: Seq[Int] =
      (0 until r).map: i =>
        nCr(q - i, r - i)
    val mainLcm: Int =
      ncr.foldLeft(1)(lcm)
    val globalResiduesAllowed: mutable.Seq[Boolean] =
      mutable.Seq.fill(mainLcm):
        true
    for 
      i <- 0 until r
    do
      val localResiduesAllowed: Array[Boolean] =
        (0 until ncr(i)).map: n =>
          localCondition(lambda, r, q, n, i)
        .toArray
      for
        n <- 0 until mainLcm
      do 
        if !localResiduesAllowed(n % ncr(i)) then
          globalResiduesAllowed(n) = false
    mainLcm -> (0 until mainLcm).filter: n =>
      globalResiduesAllowed(n)
