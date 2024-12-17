package com.fdilke.truchet

class TruchetGrid(
  width: Int,
  height: Int
):
  val squares: Seq[Square] =
    for
      x <- 0 until width : Seq[Int]
      y <- 0 until height : Seq[Int]
      index = (y * width) + x
    yield
      Square(x, y, index)

case class Square(
  xPosition: Int, 
  yPosition: Int,
  index: Int
):
  ()

