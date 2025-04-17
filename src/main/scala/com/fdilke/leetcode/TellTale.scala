package com.fdilke.leetcode

import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.{bits, log2, xorNumbers}

object TellTale:
  private def countTrue(
    values: Seq[Boolean]
  ): Int =
    values.count(identity)
    
  def egan(
    board: Seq[Seq[Boolean]]
  ): (Int, Int) =
    val oddnessRows: Seq[Boolean] =
      board.map: row =>
        countTrue(row) % 2 == 1
    val oddRows: Seq[Int] =
      oddnessRows.indices.filter: i =>
        oddnessRows(i)
    val row = xorNumbers(oddRows)
    val oddColumns: Seq[Int]  =
      board.indices.filter: j =>
        countTrue(
          board.map: row => 
            row(j)
        ) % 2 == 1
    val column = xorNumbers(oddColumns)
    (row, column)

  def my(
    board: Seq[Seq[Boolean]]
  ): (Int, Int) =
    val power2 = board.size
    val a = log2(power2)
    def coordsToSet(x: Int, y: Int): Set[Int] =
      bits(x + power2*y).toSet
    def setToCoords(set: Set[Int]): (Int, Int) =
      val value = 
        set.map(SetsUtilities.power(2, _)).sum
      val x = value % power2
      val y = value / power2
      (x, y)
    val coordSet: Set[(Int, Int)] =
      for
        x <- board.indices.toSet
        y <- board.indices if board(x)(y)
      yield
        (x, y)
    setToCoords:
      SetsUtilities.bulkXor:
        coordSet.map(coordsToSet)
    
      
