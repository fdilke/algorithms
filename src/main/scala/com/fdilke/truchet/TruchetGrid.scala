package com.fdilke.truchet

trait SquareHolder:
  val squares: Seq[Square]
  def indexFor(x: Int, y: Int): Int
  def lookup(x: Int, y: Int): Square

class TruchetGrid(
  width: Int,
  height: Int
) extends SquareHolder:
  override val squares: Seq[Square] =
    for
      x <- 0 until width : Seq[Int]
      y <- 0 until height : Seq[Int]
    yield
      Square(x, y, indexFor(x, y))

  override def indexFor(x: Int, y: Int): Int =
    (x * height) + y

  override def lookup(x: Int, y: Int): Square =
    squares(indexFor(x, y))

class Square(
  val xPosition: Int,
  val yPosition: Int,
  val index: Int
):
  ()

