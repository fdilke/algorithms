package com.fdilke.leetcode

import com.fdilke.utility.RichFunSuite._
import com.fdilke.utility.SetsUtilities._
import munit.FunSuite

import java.util.concurrent.atomic.AtomicBoolean

class EganTellTaleSpec extends TellTaleSpec(TellTale.egan)
class MyTellTaleSpec extends TellTaleSpec(TellTale.my)

class TellTaleSpec(
  tellTale: Seq[Seq[Boolean]] => (Int, Int)
) extends FunSuite:
  private def flipSquare(
    board: Seq[Seq[Boolean]],
    x: Int,
    y: Int
  ): Seq[Seq[Boolean]] =
    board.zipWithIndex.map: (row, index) =>
      if (index == x)
        row.zipWithIndex.map: (cell, j) =>
          if (j == y)
            !cell
          else
            cell
      else
        row

  private def checkBoard(
    board: Seq[Seq[Boolean]]
  ): Unit =
    val n = board.size
    val (x, y) = tellTale(board)
    val specialBoard: Seq[Seq[Boolean]] =
      flipSquare(board, x, y)
    val (p, q) = ((n/2 + 1) % n, n/2 - 1)
    val changedBoard: Seq[Seq[Boolean]] =
      flipSquare(specialBoard, p, q)
    tellTale(changedBoard) is (p, q)

  test("calculate telltale square"):
    checkBoard(
      Seq(
        Seq(true, false, true, true, false, true, false, true),
        Seq(true, false, true, false, true, false, true, false),
        Seq(false, false, true, false, false, true, true, false),
        Seq(true, false, true, true, true, false, true, false),
        Seq(true, false, true, false, false, true, true, true),
        Seq(false, true, false, true, false, true, false, true),
        Seq(false, false, true, false, false, true, true, false),
        Seq(true, false, true, true, true, false, true, false)
      )
    )

  test("calculate telltale square (2)"):
    checkBoard(
      Seq(
        Seq(true, false, true, true),
        Seq(true, false, true, false),
        Seq(false, false, true, false),
        Seq(true, false, true, true)
      )
    )

  test("calculate telltale square (3)"):
    checkBoard(
      Seq(
        Seq(true, false),
        Seq(true, false)
      )
    )
