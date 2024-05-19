package com.fdilke.utility

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
      } :_*
    )

  def sequencesOfLength[H](
    letters: Seq[H],
    length: Int
  ): Iterable[Seq[H]] =
    if (letters.isEmpty || length == 0) then
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
    val toOrder: Seq[Int] = (0 until order)
    val toArity: Seq[Int] = (0 until arity)

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
      element <- (set : Iterable[H])
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
