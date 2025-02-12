package com.fdilke.backtrack.apps

import com.fdilke.backtrack.BacktrackIterable

object NQueens extends App:
  val order = 6

  type BOARD = Set[(Int, Int)]
  val columns: Set[Int] = (0 until order).toSet
  val positions: Set[(Int, Int)] =
    for
      x <- columns
      y <- columns
    yield
      (x, y)

  def attacks(
    position: (Int, Int),
    target: (Int, Int)
  ): Boolean =
    val (a, b) = position
    val (x, y) = target
    a == x || b == y ||
      Math.abs(a - x) == Math.abs(b - y)

  def attacks(
    board: BOARD,
    position: (Int, Int)
  ): Boolean =
    board.exists: target =>
      attacks(position, target)

  def showBoard(
    board: BOARD
  ): Unit =
    println("-----------------")
    for
      i <- columns
    do
      for
        j <- columns
      do
        if board.contains( i -> j ) then
          print("Q")
        else
          print("-")
      println()

  val solutions: Iterable[BOARD] =
    BacktrackIterable.dedup[BOARD, BOARD](
      Set.empty : BOARD
    ): board =>
      if board.size == order then
        Iterable(Right(board))
      else
        positions.filterNot: position =>
          board.contains(position) ||
          attacks(board, position)
        .map: position =>
          Left(board + position)

//  if false then
  solutions foreach showBoard
//  else
  println(s"${solutions.size} solutions")

