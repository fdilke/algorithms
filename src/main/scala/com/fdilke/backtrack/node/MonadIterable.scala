package com.fdilke.backtrack.node

import cats.Monad

object MonadIterable:
  implicit object MonadIterable extends Monad[Iterable]:
    override def pure[A](x: A): Iterable[A] =
      Iterable(x)

    override def flatMap[A, B](fa: Iterable[A])(f: A => Iterable[B]): Iterable[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => Iterable[Either[A, B]]): Iterable[B] =
      f(a) flatMap {
        case Left(aa) => tailRecM(aa)(f)
        case Right(b) => pure(b)
      }
  // bonus marks for making this tail-recursive
  // note: is there not an off the peg implementation of Monad[Iterable] ?