package com.fdilke.backtrack.apps

import com.fdilke.backtrack.MapBacktrack
import MapBacktrack._
import com.fdilke.utility.SetsUtilities._
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.collection.View

object CommutingOpsBruteForce extends App:
  private val order = 3
  private val quads: Seq[(Int, Int, Int, Int)] =
    allMaps(
      source = 0 until 4,
      target = 0 until order
    ).toSeq.map { (m: Map[Int, Int]) =>
      (m(0), m(1), m(2), m(3))
    }

  def selfCommutes(op: VarArgFunc[Int, Int]): Boolean =
    quads.forall { case (a, b, c, d) =>
      op(op(a, b), op(c, d)) ==
        op(op(a, c), op(b, d))
    }

  val ops: Iterable[VarArgFunc[Int, Int]] =
    allNaryOps(arity = 2, order = order)
  val count = ops.count(selfCommutes)

  println("count = " + count + " of " + ops.size + " ("
    + Math.round(100.0 * count / ops.size) + "%)"
  )
