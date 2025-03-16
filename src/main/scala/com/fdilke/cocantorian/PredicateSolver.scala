package com.fdilke.cocantorian

import java.util.concurrent.atomic.AtomicReference

object PredicateSolver:
  def apply[X](
    equation: (X => Boolean) => Boolean
  ): Option[Map[X, Boolean]] =
    case class StumpedAtException(x: X) extends Exception
    // Boolean, Option[Map[X, Boolean]]
//      T = Boolean
//      U = Option[Map[X, Boolean]]
    def determine[Q](
       f: Boolean => Q,
       predicate: Q => Boolean
    ): Option[(Boolean, Q)] =
      val holder = AtomicReference[(Boolean, Q)]()
        if Set(true, false).exists: b =>
          val q: Q = f(b)
          val good = predicate(q)
          if (good)
            holder.set(b -> q)
          good
        then
          Some(holder.get())
        else 
          None
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
          ).flatMap {
            _._2
          }
      }
    tryMap(Map.empty)