package com.fdilke.blocks

import com.fdilke.utility.Reiterable

import java.util.function.Predicate
import scala.collection.mutable

trait SteinerSolver:
  def apply[T](
    inputs: Reiterable[T],
    solutionSize: Int,
    compatible: Predicate[(T, T)]
  ): Option[Set[T]]

object GreedySteinerSolver extends SteinerSolver:
  override def apply[T](
    inputs: Reiterable[T],
    solutionSize: Int,
    compatible: Predicate[(T, T)]
  ): Option[Set[T]] =
    val compatibles: mutable.Buffer[T] =
      mutable.Buffer[T]()
    val iterator: Iterator[T] =
      inputs.iterator
    while iterator.hasNext && compatibles.size < solutionSize
    do
      val input = iterator.next()
      if compatibles.forall:
        compatible.test(input, _)
      then compatibles += input
    if compatibles.size < solutionSize then
      None
    else
      Some(compatibles.toSet)

