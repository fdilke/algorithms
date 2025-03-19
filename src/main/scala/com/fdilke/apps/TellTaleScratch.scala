package com.fdilke.apps

import com.fdilke.backtrack.BacktrackIterable
import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.{bitCount, power}

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

object QsetScratch extends App:
  def showBits(n: Int): String =
    if (n == 0)
      ""
    else if (n % 2 == 1)
      "X" + showBits(n/2)
    else
      "_" + showBits(n/2)
  for
    i <- 0 to 1 << 8 if bitCount(i) == 4
  do
    println(showBits(i).padTo(20, '_') + '\t' + SetsUtilities.bits(i).mkString(":"))
    
/*
XXXX________________	0:1:2:3
XXX_X_______________	0:1:2:4
XX_XX_______________	0:1:3:4
X_XXX_______________	0:2:3:4
_XXXX_______________	1:2:3:4
XXX__X______________	0:1:2:5
XX_X_X______________	0:1:3:5
X_XX_X______________	0:2:3:5
_XXX_X______________	1:2:3:5
XX__XX______________	0:1:4:5
X_X_XX______________	0:2:4:5
_XX_XX______________	1:2:4:5
X__XXX______________	0:3:4:5
_X_XXX______________	1:3:4:5
__XXXX______________	2:3:4:5
XXX___X_____________	0:1:2:6
XX_X__X_____________	0:1:3:6
X_XX__X_____________	0:2:3:6
_XXX__X_____________	1:2:3:6
XX__X_X_____________	0:1:4:6
X_X_X_X_____________	0:2:4:6
_XX_X_X_____________	1:2:4:6
X__XX_X_____________	0:3:4:6
_X_XX_X_____________	1:3:4:6
__XXX_X_____________	2:3:4:6
XX___XX_____________	0:1:5:6
X_X__XX_____________	0:2:5:6
_XX__XX_____________	1:2:5:6
X__X_XX_____________	0:3:5:6
_X_X_XX_____________	1:3:5:6
__XX_XX_____________	2:3:5:6
X___XXX_____________	0:4:5:6
_X__XXX_____________	1:4:5:6
__X_XXX_____________	2:4:5:6
___XXXX_____________	3:4:5:6
XXX____X____________	0:1:2:7
XX_X___X____________	0:1:3:7
X_XX___X____________	0:2:3:7
_XXX___X____________	1:2:3:7
XX__X__X____________	0:1:4:7
X_X_X__X____________	0:2:4:7
_XX_X__X____________	1:2:4:7
X__XX__X____________	0:3:4:7
_X_XX__X____________	1:3:4:7
__XXX__X____________	2:3:4:7
XX___X_X____________	0:1:5:7
X_X__X_X____________	0:2:5:7
_XX__X_X____________	1:2:5:7
X__X_X_X____________	0:3:5:7
_X_X_X_X____________	1:3:5:7
__XX_X_X____________	2:3:5:7
X___XX_X____________	0:4:5:7
_X__XX_X____________	1:4:5:7
__X_XX_X____________	2:4:5:7
___XXX_X____________	3:4:5:7
XX____XX____________	0:1:6:7
X_X___XX____________	0:2:6:7
_XX___XX____________	1:2:6:7
X__X__XX____________	0:3:6:7
_X_X__XX____________	1:3:6:7
__XX__XX____________	2:3:6:7
X___X_XX____________	0:4:6:7
_X__X_XX____________	1:4:6:7
__X_X_XX____________	2:4:6:7
___XX_XX____________	3:4:6:7
X____XXX____________	0:5:6:7
_X___XXX____________	1:5:6:7
__X__XXX____________	2:5:6:7
___X_XXX____________	3:5:6:7
____XXXX____________	4:5:6:7
    
    */