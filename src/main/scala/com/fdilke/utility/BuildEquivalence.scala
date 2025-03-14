package com.fdilke.utility
import scala.language.postfixOps

// A 0-based, functional tribute to the original Numerical Recipes
// algorithm: http://www.aip.de/groups/soe/local/numres/bookcpdf/c8-6.pdf
// We return a sequence of 'canonical equivalents' for each number.
// You can use classes() to remap this to a lexicographically minimal set of equivalence class indices.

object BuildEquivalence {

  def apply(
             size: Int,
             relators: Iterable[(Int, Int)]
           ): Seq[Int] =
    val range =
      0 until size

    val array: Array[Int] =
      range toArray

    val trackUp: Int => Int =
      IterateToFixed(_)(array)

    def equate(
      i: Int,
      j: Int
    ) =
      if (i != j)
        array(i) = j

    for {
      (j, k) <- relators
    } equate(
      trackUp(j),
      trackUp(k)
    )

    for { i <- range } array(i) = trackUp(array(i))

    array.toIndexedSeq

  def classes(
    size: Int,
    relators: Iterable[(Int, Int)]
  ): Seq[Int] =
    toClasses(apply(size, relators))

  def toClasses(
    representatives: Seq[Int]
  ): Seq[Int] =
    val distinctReps: Seq[Int] = representatives.distinct
    val indexMap: Map[Int, Int] =
      distinctReps.zipWithIndex.toMap
    representatives.map(indexMap)
}

