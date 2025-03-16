package com.fdilke.cocantorian

import java.util.concurrent.atomic.AtomicReference

object PredicateSolver:
  def apply[X](
    equation: (X => Boolean) => Boolean
  ): Option[Map[X, Boolean]] =
    case class StumpedAtException(x: X) extends Exception
    def determine[Q](
       f: Boolean => Q,
       predicate: Q => Boolean
    ): Option[Q] =
        Seq(true, false).map(f).find(predicate)
    // ^ todo: inline this
    def tryMap(
      map: Map[X, Boolean]
    ): Option[Map[X, Boolean]] =
      (try {
        Left(
          if (equation(x =>
            map.getOrElse(
              x,
              throw StumpedAtException(x)
            )
          ))
            Some(map)
          else
            None
        )
      } catch {
        case StumpedAtException(x) =>
          Right(x)
      }) match {
        case Left(result) => result
        case Right(x) =>
          determine[Option[Map[X, Boolean]]](
            c =>
              tryMap(
                map + (x -> c)
              ),
            om => om.isDefined
          ).flatten
      }
    tryMap(Map.empty)