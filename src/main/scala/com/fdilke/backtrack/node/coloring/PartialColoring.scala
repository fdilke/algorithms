package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.MonadIterable.*
import com.fdilke.backtrack.node.Node

case class PartialColoring(
  colors: Seq[Int],
  colorAdjacencies: Seq[Seq[Boolean]]
) extends Node[PartialColoring, Iterable, Seq[Int]]:
  lazy val distinctColors: Seq[Int] = colors.distinct
  private lazy val numVertices: Int = colors.size

  lazy val consistentBlanks: Boolean =
    val unusedColors: Set[Int] =
      (0 until numVertices).toSet diff distinctColors.toSet
    unusedColors forall: u =>
      (0 until numVertices) forall: v =>
        colorAdjacencies(u)(v) && colorAdjacencies(v)(u)

  lazy val amalgamations: Seq[(Int, Int)] =
    for
      c <- distinctColors
      d <- distinctColors if c < d && !colorAdjacencies(c)(d)
    yield
      (c, d)

  def amalgamate(
    colorPair: (Int, Int)
  ): PartialColoring =
    val (c, d) = colorPair
    val newColors: Seq[Int] =
      colors.map:
        color =>
        if (color == d) c else color
    val newColorAdjacencies =
      for
        e <- 0 until numVertices
      yield
        for
          f <- 0 until numVertices
        yield
          colorAdjacencies(e)(f) ||
          (e == c && colorAdjacencies(d)(f)) ||
          (f == c && colorAdjacencies(e)(d)) ||
          e == d || f == d
    PartialColoring(
      newColors,
      newColorAdjacencies
    )

  override def explore: NodeStatus =
    amalgamations match
      case Seq() =>
        Iterable(solution(colors))
      case ams =>
        ams.map: am =>
          node(amalgamate(am))

object PartialColoring:
  def fromColorsAndAdjacencies(
    colors: Seq[Int],
    vertexAdjacencies: Seq[Seq[Boolean]]
  ): PartialColoring =
    val numVertices = colors.size
    val colorAdjacencyArray: Array[Array[Boolean]] =
      Array.fill(numVertices, numVertices)(false)
    for
      i <- 0 until numVertices
      c = colors(i)
      j <- 0 until numVertices if j != i
      d = colors(j)
    do
      if vertexAdjacencies(i)(j) then
          println(s"Joining $c,$d for $i, $j")
          colorAdjacencyArray(c)(d) = true
    val unusedColors: Set[Int] =
      (0 until numVertices).toSet diff colors.distinct.toSet
    for
      u <- unusedColors
      i <- 0 until numVertices
    do
      colorAdjacencyArray(u)(i) = true
      colorAdjacencyArray(i)(u) = true
    PartialColoring(
      colors = colors,
      colorAdjacencies =
        colorAdjacencyArray.toSeq.map:
          _.toSeq
    )
