package com.fdilke.backtrack.node.coloring

import com.fdilke.backtrack.node.MonadIterable.*
import com.fdilke.backtrack.node.NodeIterable

case class PartialColoring(
  colors: Seq[Int],
  colorAdjacencies: Seq[Seq[Boolean]]
) extends NodeIterable[Seq[Int]]:
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
