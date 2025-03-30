package com.fdilke.blocks

import com.fdilke.backtrack.BacktrackIterable
import com.fdilke.utility.{Reiterable, Reiterator}

import java.util.function.Predicate
import scala.+:
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

object PortionControlledSteinerSolver extends SteinerSolver:
  override def apply[T](
    inputs: Reiterable[T],
    solutionSize: Int,
    compatible: Predicate[(T, T)]
  ): Option[Set[T]] =
    case class Ingredients(
      set: Set[T],
      source: Reiterator[T]
    ):
      def explore: Iterable[Either[Ingredients, Set[T]]] =
        if set.size == solutionSize then
          Iterable:
            Right:
              set
        else if source.hasNext then
          val t = source.next()
          def continueWith(
            aSet: Set[T],
            aSource: Reiterator[T]
          ): Either[Ingredients, Set[T]] =
            Left:
              Ingredients(
                aSet,
                aSource
              )
          if set.forall:
            u => compatible.test(t, u)
          then
            val dupSource =
              source.duplicateState()
            Iterable(
              continueWith(set, source),
              continueWith(set + t, dupSource),
            )
          else
            Iterable(
              continueWith(set, source)
            )
        else
          Iterable.empty

    BacktrackIterable[Ingredients, Set[T]](
      Ingredients(
        set = Set.empty[T],
        source = inputs.iterator
      )
    ):
      _.explore
    .headOption