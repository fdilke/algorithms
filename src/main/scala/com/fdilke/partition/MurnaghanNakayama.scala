package com.fdilke.partition

object MurnaghanNakayama:
  def reductions(
    partition: Seq[Int],
    units: Int
  ): Seq[Seq[Int]] =
    if units == 0 then
      Seq:
        Seq.fill(partition.size)(0)
    else
      val solutions: Seq[Option[Seq[Int]]] =
        for
          row <- partition.indices
        yield
          // try to build a solution starting at this row
          var blocksLeft: Int = units
          var rowPlus : Int = row
          val array: Array[Int] =
            new Array[Int](partition.length)
          while blocksLeft > 0 && rowPlus < partition.length do
            if rowPlus < partition.length - 1 then
              val blocksToUse =
                Math.min(blocksLeft, partition(rowPlus) - partition(rowPlus + 1) + 1)
              array(rowPlus) = blocksToUse
              blocksLeft -= blocksToUse
            else // last row
              if blocksLeft <= partition(rowPlus) then
                array(rowPlus) = blocksLeft
                blocksLeft = 0
              else // no solution
                blocksLeft = -1
            rowPlus += 1
          if blocksLeft == 0 && (
            (rowPlus == 0) || (rowPlus == partition.length) ||
            (partition(rowPlus) <= partition(rowPlus - 1) - array(rowPlus - 1))
          ) then
            Some(array.toSeq)
          else
            None
      solutions.flatten

  def height(
    reduction: Seq[Int]
  ): Int =
    reduction.count:
      _ > 0
    - 1
