package com.fdilke.truchet

import com.fdilke.truchet.Orientation.Forward
import com.fdilke.utility.BuildEquivalence
import com.fdilke.utility.NumberCrunch.confineTo

import java.awt.{Color, Dimension, Graphics}
import reflect.Selectable.reflectiveSelectable
import scala.language.reflectiveCalls
import scala.util.Random

trait SquareHolder:
  val squares: Seq[Square]
  def indexFor(x: Int, y: Int): Int
  def lookup(x: Int, y: Int): Square
  def conditionalLookup(x: Int, y: Int): Option[Square]
  def draw(graphics: Graphics, dimension: Dimension): Unit

trait TileHolder:
  val tiles: Seq[Tile]
  val tileAdjacencies: Seq[(Tile, Tile)]
  val tileRegions: Seq[Int]
  val regionColors: Seq[Color]
  val regionAdjacencies: Seq[Seq[Boolean]]

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
  boolStream: BoolStream,
  colorGenerator: () => Color
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

  override val tileRegions: Seq[Int] =
    BuildEquivalence.classes(
      tiles.size,
      tileAdjacencies map:
        (t, u) => (t.index, u.index)
    )
    
  override val regionColors: Seq[Color] =
    tileRegions.distinct.indices.map:
      _ => colorGenerator()

  override val regionAdjacencies: Seq[Seq[Boolean]]  =
    val adjacencyRecords: Seq[(Int, Int)] =
      squares.map:
        square =>
          def regionFor(tileNo: Int) = tileRegions(square.tiles(tileNo).index)
          regionFor(0) -> regionFor(1)
      .filterNot:
        (r, s) => r == s
    val fullRecords: Seq[(Int, Int)] =
      adjacencyRecords ++ adjacencyRecords.map { case (r, s) => s -> r }
//    println("Initial adj records = " + adjacencyRecords)
//    for
//      r <- regionColors.indices
//    do
//      for
//        s <- regionColors.indices
//      do
//        if (fullRecords.contains (r -> s))
//          print("X")
//        else
//          print("_")
//      println("")
    for
      r <- regionColors.indices
    yield
      for
        s <- regionColors.indices
      yield
        fullRecords.contains (r -> s)
    
//  println("Tile adjacencies = " + tileAdjacencies.map { case (t, u) => (t.index, u.index)})
//  println("There are " + regionColors.size + " regions")

  private def colorTile(tile: Tile): Color =
    regionColors(tileRegions(tile.index))

  override def draw(graphics: Graphics, dimension: Dimension): Unit =
    graphics.setColor(Color.BLACK)
    val squareWidth = dimension.width / width
    val squareHeight = dimension.height / height
    for
      square <- squares
    do
      square.draw(graphics, squareWidth, squareHeight, colorTile)

object TruchetGrid:
  private val randomColors = Random(0L)
  private def colorIndexGenerator(): Int =
    randomColors.nextInt(256)
  def colorGenerator(): Color =
    Color(colorIndexGenerator(), colorIndexGenerator(), colorIndexGenerator())

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

  val upTile: Tile =
    Tile(index * 2)

  val downTile: Tile =
    Tile(index*2 + 1)

  val tiles: Seq[Tile] =
    Seq(upTile, downTile)

  def leftTile: Tile =
    if (orientation == Forward)
      upTile
    else
      downTile

  def rightTile: Tile =
    if (orientation == Forward)
      downTile
    else
      upTile

  def tileAdjacencies: Seq[(Tile, Tile)] =
      (for
        square <- right.toSeq
        other = square.leftTile
      yield
        (rightTile, other)
      ) ++
      (for
        square <- down.toSeq
        other = square.upTile
      yield
        (downTile, other)
      )

  def draw(graphics: Graphics, squareWidth: Int, squareHeight: Int, colorRegion: Tile => Color): Unit =
    def xx(x: Int) =
      squareWidth * (xPosition + x)
    def yy(y: Int) =
      squareHeight * (yPosition + y)
    def subFill(
      tile: Tile,
      x0: Int, x1: Int, x2: Int,
      y0: Int, y1: Int, y2: Int
     ): Unit =
      graphics.setColor(colorRegion(tile))
      graphics.fillPolygon(
        Array[Int]( xx(x0), xx(x1), xx(x2) ),
        Array[Int]( yy(y0), yy(y1), yy(y2) ),
        3
      )
    def drawDiagonal(color: Color, fromX: Int, fromY: Int, toX: Int, toY: Int): Unit =
      graphics.setColor(color)
      graphics.drawLine(
        xx(fromX), yy(fromY),
        xx(toX), yy(toY)
      )

    if (orientation == Forward)
      subFill(
        upTile,
        0, 1, 0,
        0, 0, 1
      )
      subFill(
        downTile,
        1, 0, 1,
        1, 1, 0
      )
      drawDiagonal(Color.BLACK, 1, 0, 0, 1)
    else
      subFill(
        upTile,
        0, 1, 1,
        0, 1, 0
      )
      subFill(
        downTile,
        1, 0, 0,
        1, 0, 1
      )
      drawDiagonal(Color.BLACK, 0, 0, 1, 1)

class Tile(
  val index: Int
)

