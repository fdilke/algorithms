package com.fdilke.utility

import java.util.concurrent.atomic.AtomicReference
import java.util.function.{Predicate, UnaryOperator}

class Reiterable[T](
  initial:T,
  nextOperator: UnaryOperator[T],
  continuePredicate: Predicate[T]
) extends Iterable[T]:
  override def iterator: Iterator[T] =
    new Iterator[T]:
      val value: AtomicReference[T] =
        AtomicReference(initial)
      override def hasNext: Boolean =
        continuePredicate.test(value.get)
      override def next: T =
        value.getAndUpdate(nextOperator)
