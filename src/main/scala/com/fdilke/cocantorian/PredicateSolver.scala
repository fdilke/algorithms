package com.fdilke.cocantorian

import com.fdilke.backtrack.BacktrackIterable

import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable

trait PredicateSolver:
  def apply[X](
    equation: (X => Boolean) => Boolean
  ): Option[Map[X, Boolean]]

object ExPredicateSolver extends PredicateSolver:
  override def apply[X](
    equation: (X => Boolean) => Boolean
  ): Option[Map[X, Boolean]] =
    case class StumpedAtException(x: X) extends Exception
    def tryMap(
      map: Map[X, Boolean]
    ): Option[Map[X, Boolean]] =
      (try
        Left:
          if equation: x =>
            map.getOrElse(
              x,
              throw StumpedAtException(x)
            )
          then
            Some(map)
          else
            None
      catch
        case StumpedAtException(x) =>
          Right(x)
      ) match
        case Left(result) => result
        case Right(x) =>
          Seq(true, false).map: c =>
              tryMap:
                map + (x -> c)
          .find:
            _.isDefined
          .flatten
    tryMap:
      Map.empty

object ExFreePredicateSolver extends PredicateSolver:
  override def apply[X](
    equation: (X => Boolean) => Boolean
  ): Option[Map[X, Boolean]] =
    def tryMap(
      map: Map[X, Boolean]
    ): (Seq[X], Boolean) =
      val buffer: mutable.Buffer[X] = mutable.Buffer()
      val result: Boolean =
        equation: x =>
          map.get(x) match
            case Some(flag) => flag
            case None =>
              buffer += x
              false
      buffer.toSeq -> result
    BacktrackIterable[Map[X, Boolean], Map[X, Boolean]](
      Map.empty
    ): map =>
      tryMap(map) match
        case (seq, true) =>
          Iterable:
            Right:
              map ++ seq.map { _ -> false }.toMap
        case (seq, false) =>
          for
            (x, i) <- seq.zipWithIndex
          yield
            Left:
              map ++ (seq.take(i).map { _ -> false} :+ (x -> true)).toMap
    .headOption

