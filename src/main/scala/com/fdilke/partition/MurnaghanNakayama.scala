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
//      val sum = partition.sum
      val solutions: Seq[Option[Seq[Int]]] =
        for
          row <- partition.indices
        yield
          // try to build a solution starting at this row
          var blocksLeft: Int = units
          var rowPlus : Int = row
          val array: Array[Int] =
            new Array[Int](partition.length)
//          println(s"looping for row $row: blocksLeft=$blocksLeft, rowPlus=$rowPlus, PL=${partition.length}")
          while blocksLeft > 0 && rowPlus < partition.length do
//            println(s"looping 1")
            if rowPlus < partition.length - 1 then
              val blocksToUse =
                Math.min(blocksLeft, partition(rowPlus) - partition(rowPlus + 1) + 1)
//              println(s"filling in: $blocksToUse")
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
//            println(s"disallowing ${array.toSeq} with blocksLeft=$blocksLeft, rowPlus=$rowPlus, pl=${partition.length} ...")
//            println(s"... partition=$partition")
            None
      solutions.flatten
