package com.fdilke.backtrack.node

import cats.Monad

import scala.annotation.tailrec
import scala.util.Try

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
    @tailrec def loop(
      value: Iterable[Either[A, B]]
    ): Iterable[B] =
      if value.forall { _.isRight } then
        value.collect:
          case Right(b) => b
      else
        loop(
          value.flatMap:
            case Left(aa) => f(aa)
            case Right(b) => pure(Right(b))
        )
    loop(f(a))
