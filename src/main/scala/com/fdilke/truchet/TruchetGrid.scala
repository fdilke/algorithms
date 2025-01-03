package com.fdilke.truchet

import com.fdilke.truchet.Orientation.Forward
import com.fdilke.utility.BuildEquivalence
import com.fdilke.utility.NumberCrunch.confineTo

import reflect.Selectable.reflectiveSelectable
import scala.language.reflectiveCalls

trait SquareHolder:
  val squares: Seq[Square]
  def indexFor(x: Int, y: Int): Int
  def lookup(x: Int, y: Int): Square
  def conditionalLookup(x: Int, y: Int): Option[Square]

trait TileHolder:
  val tiles: Seq[Tile]
  val tileAdjacencies: Seq[(Tile, Tile)]
  val regions: Seq[Int]
  

type BoolStream = { def nextBoolean(): Boolean }

enum Orientation(repr: String):
  case Forward extends Orientation("/")
  case Backward extends Orientation("\\")
  override def toString: String =
    repr

object Orientation:
  def from(bool: Boolean): Orientation =
    if bool then
      Forward
    else
      Backward

class TruchetGrid(
  width: Int,
  height: Int,
  toroidal: Boolean,
  boolStream: BoolStream
) extends SquareHolder with TileHolder:
  grid =>
  override val squares: Seq[Square] =
    for
      x <- 0 until width : Seq[Int]
      y <- 0 until height : Seq[Int]
      orientation = Orientation.from(boolStream.nextBoolean())
    yield
      Square(x, y, indexFor(x, y), grid, orientation)

  override val tiles: Seq[Tile] =
    squares.flatMap:
      square =>
      square.tiles

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

  override def toString: String =
    (for
      y <- 0 until height
      x <- 0 until width
    yield
      lookup(x, y).orientation.toString +
        (if x == width - 1 then "\n" else "")
    ).mkString("")

  override val tileAdjacencies: Seq[(Tile, Tile)] =
    for
      square <- squares
      adjacency <- square.tileAdjacencies
    yield
      adjacency

  override val regions: Seq[Int] =
    BuildEquivalence.classes(
      tiles.size,
      tileAdjacencies map:
        case (t, u) => (t.index, u.index)  
    )

class Square(
  val xPosition: Int,
  val yPosition: Int,
  val index: Int,
  holder: SquareHolder,
  val orientation: Orientation
):
  def left: Option[Square] =
    holder.conditionalLookup(xPosition - 1, yPosition)
  def right: Option[Square] =
    holder.conditionalLookup(xPosition + 1, yPosition)
  def up: Option[Square] =
    holder.conditionalLookup(xPosition, yPosition - 1)
  def down: Option[Square] =
    holder.conditionalLookup(xPosition, yPosition + 1)
  val tiles: Seq[Tile] =
    Seq(
      Tile(index*2),
      Tile(index*2 + 1)
    )
    
  def leftTile: Tile =
    if (orientation == Forward)
      tiles(0)
    else
      tiles(1)
      
  def upTile: Tile =
    tiles(0)
      
  def tileAdjacencies: Seq[(Tile, Tile)] =
    if orientation == Forward then
      (for
        square <- right.toSeq
        other = square.leftTile
      yield
        (tiles(1), other)
      ) ++
      (for
        square <- down.toSeq
        other = square.upTile
      yield
        (tiles(1), other)
      )
    else
      (for
        square <- right.toSeq
        other = square.leftTile
      yield
        (tiles(0), other)
      ) ++
      (for
        square <- down.toSeq
        other = square.upTile
      yield
        (tiles(1), other)
      )

class Tile(
  val index: Int
):
  ()

