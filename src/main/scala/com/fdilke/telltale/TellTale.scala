package com.fdilke.telltale

object TellTale:
  private def countTrue(
    values: Seq[Boolean]
  ): Int =
    values.count(identity)
    
  def egan(
    board: Seq[Seq[Boolean]]
  ): (Int, Int) =
    val wzzzt: Seq[Boolean] =
      board.map: row =>
        countTrue(row) % 2 == 1
    val oddRows: Seq[Int] =
      wzzzt.indices.filter: i =>
        wzzzt(i)
    val row = oddRows.reduce{ _ ^ _ }
    val oddColumns: Seq[Int]  =
      board.indices.filter: j =>
        countTrue(
          board.map: row => 
            row(j)
        ) % 2 == 1
    val column = oddColumns.reduce { _ ^ _ }
    (row, column)
    
    
      
