package com.fdilke.truchet

class TruchetGrid(
  width: Int,
  height: Int
):
  val squares: Seq[Square] =
    for
      x <- 0 until width : Seq[Int]
      y <- 0 until height : Seq[Int]
    yield
      Square(x, y, indexFor(x, y))

  def indexFor(x: Int, y: Int): Int =
    (x * height) + y

  def lookup(x: Int, y: Int): Square =
    squares(indexFor(x, y))

case class Square(
  xPosition: Int,
  yPosition: Int,
  index: Int
):
  ()

