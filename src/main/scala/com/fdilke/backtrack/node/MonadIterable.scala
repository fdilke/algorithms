package com.fdilke.backtrack.node

import cats.Monad

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.Stack

implicit object MonadIterable extends Monad[Iterable]:
  override def pure[A](
    x: A
  ): Iterable[A] =
    Iterable(x)

  override def map[A, B](
    fa: Iterable[A]
  )(
    f: A => B
  ): Iterable[B] =
    fa.map(f)

  override def flatMap[A, B](
    fa: Iterable[A]
  )(
    f: A => Iterable[B]
  ): Iterable[B] =
    fa.flatMap(f)

  override def tailRecM[A, B](
    a: A
  )(
    f: A => Iterable[Either[A, B]]
  ): Iterable[B] =
    new Iterable[B]:
      override def iterator: Iterator[B] =
        new Iterator[B]:
          private val inputs: mutable.Stack[A] = new mutable.Stack[A]
          inputs.push(a)
          private val outputs: mutable.Stack[B] = new mutable.Stack[B]
          override def hasNext: Boolean =
            while
              outputs.isEmpty && inputs.nonEmpty
            do
              parseEithers(f(inputs.pop())):
                (running, converged) =>
                inputs.pushAll(running)
                outputs.pushAll(converged)
            outputs.nonEmpty

          override def next(): B =
            if !hasNext then
              throw new NoSuchElementException()
            else
              outputs.pop()

  private def parseEithers[X, Y, Z](
    eithers: Iterable[Either[X, Y]]
  )(
    f: (Iterable[X], Iterable[Y]) => Z
  ): Z =
    val (
      runningI: Iterable[Either[X, Y]],
      convergedI: Iterable[Either[X, Y]]
    ) =
    eithers.partition:
      _.isLeft
    val running: Iterable[X] =
      runningI.map:
        _.swap.toOption.get
    val converged: Iterable[Y] =
      convergedI.map:
        _.toOption.get
    f(running, converged)

  def failRecM[A, B](
    a: A
  )(
    f: A => Iterable[Either[A, B]]
  ): Iterable[B] =
    Iterable.unfold[
      Iterable[B],
      Iterable[A]
    ](Iterable(a)): (s: Iterable[A]) =>
      if (s.isEmpty)
        None
      else
        val eithers: Iterable[Either[A, B]] = s flatMap f
        parseEithers(eithers):
          (running, converged) =>
          Some(converged -> running)
    .flatten

  private def failRecM2[A, B](
    a: A
  )(
    f: A => Iterable[Either[A, B]]
  ): Iterable[B] =
    def loop(
      value: Iterable[Either[A, B]]
    ): Iterable[B] =
      val (
        runningI: Iterable[Either[A, B]],
        convergedI: Iterable[Either[A, B]]
      ) =
        value.partition:
          _.isLeft
      val running: Iterable[A] =
          runningI.map:
            _.swap.toOption.get
      val converged: Iterable[B] =
          convergedI.map:
            _.toOption.get

      if running.isEmpty then
        converged
      else
        converged ++ running.flatMap:
          a => loop(f(a))

    loop(f(a))

//      if value.forall { _.isRight } then
//        value.collect:
//          case Right(b) => b
//      else
//        loop(
//          value.flatMap:
//            case Left(aa) => f(aa)
//            case Right(b) => pure(Right(b))
//        )
//    loop(f(a))
