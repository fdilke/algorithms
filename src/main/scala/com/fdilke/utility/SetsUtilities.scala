package com.fdilke.utility

import scala.annotation.targetName
import scala.language.postfixOps
import scala.reflect.ClassTag

// edited snapshot of bewl2's SetsUtilities

object SetsUtilities:
  def allMaps[A, B](
     source: Iterable[A],
     target: Iterable[B]
  ): Iterable[Map[A, B]] =
    if (source.isEmpty)
      Iterable(Map.empty)
    else
      (for {
        partialMap <- allMaps(source.tail, target)
        choice <- target
      } yield {
        partialMap + (source.head -> choice)
      }).view

  trait VarArgFunc[-A, +B]:
    def apply(is: A*): B

  private def seqToMap[T](
    a: Seq[T]
  ): Map[Int, T] =
    Map(
      a.indices map { i =>
        i -> a(i)
      }*
    )

  def sequencesOfLength[H](
    letters: Seq[H],
    length: Int
  ): Iterable[Seq[H]] =
    if letters.isEmpty || length == 0 then
      Iterable(Seq.empty)
    else
      for
        seq <- sequencesOfLength(letters, length - 1)
        letter <- letters
      yield
        letter +: seq

  def wordsOfLength(
    letters: String,
    length: Int
  ): Iterable[String] =
    sequencesOfLength(letters, length) map { _.string }

  def allNaryOps(
    arity: Int,
    order: Int
  ): Iterable[VarArgFunc[Int, Int]] =
    val toOrder: Seq[Int] = 0 until order
    val toArity: Seq[Int] = 0 until arity

    allMaps(
      allMaps(toArity, toOrder),
      toOrder
    ) map { (m: Map[Int, Int] => Int) =>
      (a: Seq[Int]) =>
        m(seqToMap(a))
    }

  def subsetsOf[H](set: Set[H]): Iterable[Set[H]] =
    if (set.isEmpty)
      Iterable(Set.empty[H])
    else for
      element <- set : Iterable[H]
      theRest <- subsetsOf(set - element)
      both <- Iterable[Set[H]](theRest, theRest + element)
    yield both

  def subsetsOfString(letters: String): Iterable[String] =
    subsetsOf(letters.toSet) map:
      _.toSeq.string

  def bulkJoin[X, Y, Z](
    inputs: Seq[X],
    candidates: X => Iterable[Y],
    assignment: (X, Y) => Z,
    assignmentZero: Z,
    join: (Z, Z) => Z
  ): Iterable[Z] =
    inputs.foldLeft(Iterable[Z](assignmentZero)):
      (it: Iterable[Z], x: X) =>
      for
        y <- candidates(x)
        assign = assignment(x, y)
        z <- it
      yield
        join(z, assign)

  def naivePow(n: Int, k: Int): Int =
    Seq.fill(n)(k).product
  extension(letters: Seq[Char])
    inline def string: String =
      new String(letters.toArray)

  def intSqrt(square: Int): Int =
    ((0 to square) find: n =>
      n * n == square
    ).getOrElse
      { throw new IllegalArgumentException(s"$square is not a square") }

  def squareUp[X](
    square: X*
  ): Seq[Seq[X]] =
    val root = intSqrt(square.size)
    for
      i <- 0 until root
    yield
      for
        j <- 0 until root
      yield
        square(i * root + j)

  def invertPermutation(
    permutation: Seq[Int]
  ): Seq[Int] =
    val inverse: Array[Int] =
      Array[Int](permutation*)
    for
      i <- permutation.indices
    do
      inverse(permutation(i)) = i
    inverse.toSeq

  def crossCheckResult[A](
    computations: Seq[() => A]
  ): Option[A] =
    computations match
      case Nil =>
        throw new IllegalArgumentException("no computations provided")
      case first +: tail =>
        val firstResult: A = first()
        if tail.forall:
          _() == firstResult
        then Some(firstResult)
        else None

  @targetName("crossCheckResultsVarargs")
  inline def crossCheckResult[A](
    computations: () => A *
  ): Option[A] =
    crossCheckResult(computations)
  
  def crossCheckResultOptional[A](
    computations: Seq[() => Option[A]]
  ): Option[A] =
    computations match
      case Nil =>
        None
      case first +: tail =>
        first() match
          case None =>
            None
          case someFirst =>
            if tail.forall:
              _() == someFirst
            then someFirst
            else None

  @targetName("crossCheckResultsOptionalVarargs")
  inline def crossCheckResultOptional[A](
    computations: () => Option[A] *
  ): Option[A] =
    crossCheckResultOptional(computations)

  def allOrNone[A: ClassTag](
    computations: Seq[() => Option[A]]
  ): Option[Seq[A]] =
    val arrayOfA: Array[A] =
      new Array[A](computations.size)
    if
      computations.zipWithIndex.exists:
        (block, i) =>
          block() match
            case None => true
            case Some(a) =>
              arrayOfA(i) = a
              false
    then
      None
    else
      Some(arrayOfA.toSeq)

  @targetName("allOrNoneVarargs")
  inline def allOrNone[A: ClassTag](
    computations: () => Option[A] *
  ): Option[Seq[A]] =
    allOrNone(computations)

  def power(exponent: Int, n: Int): Int = 
    Seq.fill[Int](n)(exponent).product

  def binaryXor[A](
    s1: Set[A],
    s2: Set[A]
  ): Set[A] =
    (s1 union s2) diff (s1 intersect s2)

  def bulkXor[A](
    sets: Set[Set[A]]
  ): Set[A] =
    if sets.isEmpty then
      Set.empty[A]
    else
      sets.reduce[Set[A]]:
        binaryXor[A]

  def xorNumbers(
    numbers: Seq[Int]
  ): Int =
    if numbers.isEmpty then
      0
    else
      numbers.reduce:
        _ ^ _

  def bitCount(
    n: Int
  ): Int =
    if (n == 0)
      0
    else if (n % 2 == 0)
      bitCount(n/2)
    else
      1 + bitCount(n/2)

  def bits(
    n: Int
  ): Seq[Int] =
    if (n == 0)
      Seq.empty
    else
      val recursed: Seq[Int] =
        bits(n/2).map { _ + 1 }
      if (n % 2 == 0)
        recursed
      else
        0 +: recursed

  def log2(
    power2: Int
  ): Int =
    if (power2 == 1)
      0
    else if (power2 <= 0 || (power2 % 2 == 1))
      throw new IllegalArgumentException("not a power of 2")
    else
      1 + log2(power2/2)
