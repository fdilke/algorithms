package com.fdilke.truchet

import com.fdilke.utility.NumberCrunch.confineTo

trait SquareHolder:
  val squares: Seq[Square]
  def indexFor(x: Int, y: Int): Int
  def lookup(x: Int, y: Int): Square
  def conditionalLookup(x: Int, y: Int): Option[Square]

class TruchetGrid(
  width: Int,
  height: Int,
  toroidal: Boolean
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
    squares(indexFor(x, y))

  private def inBounds(z: Int, bound: Int): Boolean =
    (z >= 0) && (z < bound)
  
  override def conditionalLookup(x: Int, y: Int): Option[Square] =
    if (toroidal || (inBounds(x, width) && inBounds(y, height) ))
      Some(lookup(x, y))
    else None

class Square(
  val xPosition: Int,
  val yPosition: Int,
  val index: Int,
  holder: SquareHolder
):
  def left: Option[Square] =
    holder.conditionalLookup(xPosition - 1, yPosition)
  def right: Option[Square] =
    holder.conditionalLookup(xPosition + 1, yPosition)
  def up: Option[Square] =
    holder.conditionalLookup(xPosition, yPosition - 1)
  def down: Option[Square] =
    holder.conditionalLookup(xPosition, yPosition + 1)

