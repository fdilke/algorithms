package com.fdilke.truchet

import com.fdilke.utility.NumberCrunch.confineTo

trait SquareHolder:
  val squares: Seq[Square]
  def indexFor(x: Int, y: Int): Int
  def lookup(x: Int, y: Int): Square

class TruchetGrid(
  width: Int,
  height: Int
) extends SquareHolder:
  grid =>
  override val squares: Seq[Square] =
    for
      x <- 0 until width : Seq[Int]
      y <- 0 until height : Seq[Int]
    yield
      Square(x, y, indexFor(x, y), grid)

  override def indexFor(x: Int, y: Int): Int =
    confineTo(x, width) * height + confineTo(y, height)

  override def lookup(x: Int, y: Int): Square =
    println(s"indexFor($x, $y) is ${indexFor(x, y)}")
    squares(indexFor(x, y))

class Square(
  val xPosition: Int,
  val yPosition: Int,
  val index: Int,
  holder: SquareHolder
):
  def left: Square =
    holder.lookup(xPosition - 1, yPosition)
  def right: Square =
    holder.lookup(xPosition + 1, yPosition)
  def up: Square =
    holder.lookup(xPosition, yPosition - 1)
  def down: Square =
    holder.lookup(xPosition, yPosition + 1)

