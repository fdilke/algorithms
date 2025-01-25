package com.fdilke.backtrack.node

import cats.Monad

import scala.annotation.tailrec

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
    Iterable.unfold[
      Iterable[B],
      Iterable[A]
    ](Iterable(a)): (s: Iterable[A]) =>
      if (s.isEmpty)
        None
      else
        val eithers: Iterable[Either[A, B]] = s flatMap f
        val (
          runningI: Iterable[Either[A, B]],
          convergedI: Iterable[Either[A, B]]
        ) =
          eithers.partition:
            _.isLeft
        val running: Iterable[A] =
          runningI.map:
            _.swap.toOption.get
        val converged: Iterable[B] =
          convergedI.map:
            _.toOption.get
        Some(converged -> running)
    .flatten


  private def failRecM[A, B](
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
