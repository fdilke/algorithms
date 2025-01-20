package com.fdilke.backtrack.node.coloring

object GraphConstructions:
  def torus(
    width: Int,
    height: Int
  ): Seq[(Int, Int)] =
    if width == 0 || height == 0 then
      Seq.empty
    else
      def cellIndex(i: Int, j: Int): Int =
        (i % width) * height + (j % height)
      val pairs =
        for
          i <- 0 to width
          j <- 0 to height
        yield
          Seq(
            cellIndex(i, j) -> cellIndex(i + 1, j),
            cellIndex(i, j) -> cellIndex(i, j + 1)
          )
      pairs.flatten.distinct

//"The odd graph O_{n} has one vertex for each of the (n-1)-element subsets of a (2n-1)}-element set.
//Two vertices are connected by an edge if and only if the corresponding subsets are disjoint."

  def oddGraph(n: Int): Seq[(Int, Int)] =
    Seq.empty
