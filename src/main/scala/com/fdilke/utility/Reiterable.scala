package com.fdilke.utility

import java.util.concurrent.atomic.AtomicReference
import java.util.function.{Predicate, UnaryOperator}

class Reiterable[T](
  initial:Option[T],
  nextOperator: T => Option[T],
  continueCondition: Predicate[T]
) extends Iterable[T]:
  private class LocalReiterator(
    cursor: Option[T]                               
  ) extends Reiterator[T]:
    private val value: AtomicReference[Option[T]] =
      AtomicReference:
        cursor
    override def hasNext: Boolean =
      value.get.exists:
        continueCondition.test
    override def next: T =
      value.getAndUpdate:
        _.flatMap:
          nextOperator.apply
      .get
    override def duplicateState(): Reiterator[T] =
      LocalReiterator:
        value.get
  override def iterator: Reiterator[T] =
    LocalReiterator(initial)

object Reiterable:
  def list[T](
    inputs: T*
  ): Reiterable[T] =
    def nextIndex(u: T) =
      1 + inputs.indexOf(u)
    Reiterable[T](
      inputs.headOption,
      u =>
        val index = nextIndex(u)
        if index < inputs.length then
          Some(inputs(index))
        else
          None,
      _ => true
    )

trait Reiterator[T] extends Iterator[T]:
  def duplicateState(): Reiterator[T]
