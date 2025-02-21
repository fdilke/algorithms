package com.fdilke.apps

import com.fdilke.backtrack.BacktrackIterable
import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.power

object TellTaleFail extends App:
  for
    n <- Seq(1, 2, 4, 8, 16)
  do
    val m: Int = n * (n-1) / 2
    val power2: Int = Seq.fill[Int](n)(2).product
    println(s"power2 = $power2")
    val s: Int = power2/n
    val scount: Int =
      (0 until s).foldLeft(1):
        (h, r) =>
        val numerator = power2 - m*r
        if (h * numerator) % (r + 1) != 0 then
          throw new IllegalArgumentException("fail! not divisible")
        (h * numerator) / (r + 1)
    println(s"n = $n\tscount = $scount")

object TellTale extends App:
  for
    n <- Seq(1, 2, 4, 8, 16)
  do
    println(s"n = $n")
    val m: Int = n * (n-1) / 2
    val power2: Int = Seq.fill[Int](n)(2).product
    val s: Int = power2/n
    println(s"power2 = $power2 ; s = $s")
    type VECTOR = Set[Int]
    val xValues: Seq[Int] = 0 until n
    val vValues: Set[VECTOR] =
      SetsUtilities.subsetsOf(xValues.toSet).toSet
    val diffs: Set[VECTOR] =
      for
        x <- xValues.toSet
        y <- xValues.toSet if y != x
      yield
        Set(x, y)
    def vecAdd(v1: VECTOR, v2: VECTOR): VECTOR =
      v1.diff(v2).union(v2.diff(v1))
    def vecAddSet(vs1: Set[VECTOR], v2: VECTOR): Set[VECTOR] =
      vs1.map { vecAdd(_, v2) }
    val possibles: Iterable[Set[VECTOR]] =
      BacktrackIterable.dedup[Set[VECTOR], Set[VECTOR]](
        Set(Set.empty)
      ): vectors =>
        if vectors.size == s then
          Iterable(Right(vectors))
        else
          val toAvoid: Set[VECTOR] =
            diffs.flatMap: d =>
              vecAddSet(vectors, d)
          vValues.diff(toAvoid).map: w =>
            Left(vectors + w)
//    println(s"possibles: ${possibles.size}")
//    println("sample: " + possibles.head)
    val letters: Seq[Char] = Seq('x', 'y', 'z', 'w')
    println("possibles:")
      possibles.foreach: possible =>
        println(
          possible.map: vector =>
            vector.toSeq.sorted.map { i => letters(i) }.mkString("")
          .mkString(",")
        )

object SpecialSubspace extends App:
  for
    a <- 1 to 4
  do
    val n = power(2, a)
    val m: Int = n * (n-1) / 2
    val power2: Int = power(2, n)
    val s: Int = power2/n
    val b: Int = n - a
    println(s"n = $n, s = $s, b = $b")
    type VECTOR = Set[Int]
    val xValues: Seq[Int] = 0 until n
    val vValues: Set[VECTOR] =
      SetsUtilities.subsetsOf(xValues.toSet).toSet
    val diffs: Set[VECTOR] =
      for
        x <- xValues.toSet
        y <- xValues.toSet if y != x
      yield
        Set(x, y)
    def vecAdd(v1: VECTOR, v2: VECTOR): VECTOR =
      v1.diff(v2).union(v2.diff(v1))
    def vecAddSet(vs1: Set[VECTOR], v2: VECTOR): Set[VECTOR] =
      vs1.map { vecAdd(_, v2) }
    val toAvoid: Set[VECTOR] =
      vValues.diff(diffs)
    val specialSubspaces: Iterable[Set[VECTOR]] =
      BacktrackIterable.dedup[(Set[VECTOR], Set[VECTOR]), Set[VECTOR]](
        Set.empty -> Set(Set.empty)
      ): (basis, subspace) =>
        if basis.size == b then
//        if subspace.size == s then
          Iterable(Right(basis))
        else
          toAvoid.map: w =>
            w -> vecAddSet(subspace, w)
          .filter: (w, coset) =>
            coset.intersect(diffs).isEmpty
          .map: (w, coset) =>
            Left(basis + w, subspace.union(coset))

    //    println(s"possibles: ${possibles.size}")
    //    println("sample: " + possibles.head)
    val basis = specialSubspaces.head
    val letters: Seq[Char] = Seq('x', 'y', 'z', 'w')
    println("a special subspace basis:")
    println(basis)
