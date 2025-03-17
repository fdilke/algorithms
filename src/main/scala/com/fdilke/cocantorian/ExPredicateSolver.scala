package com.fdilke.cocantorian

import java.util.concurrent.atomic.AtomicReference

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