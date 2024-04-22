package com.fdilke.offcuts

object OffCuts:

  object HackStack extends App:
    def corecurse(depth: Int): Unit =
      recurse(depth)

    def recurse(depth: Int): Unit =
      if depth == 40 then
        println("Hit max depth")
        val trace: Seq[StackTraceElement] =
          Thread.currentThread().getStackTrace.toSeq
        println("size = " + trace.size)
        trace.foreach { tel =>
          println("tel = " + tel.toString)
        }
      else
        corecurse(depth + 1)

    recurse(0)

