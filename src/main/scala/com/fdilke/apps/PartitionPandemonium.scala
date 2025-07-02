package com.fdilke.apps

import com.fdilke.partition.Partitions
import com.fdilke.utility.Handy
import scala.math.Integral.Implicits._

object PartitionPandemonium extends App:
  def pad(x: String): String =
    x.padTo(10, ' ')
  def pad(x: Number): String =
    pad(x.toString)

  val (_, totalTimeMsec) =
    Handy.timeMsec:
      for
        n <- 0 to 100
      do
        val (count, timeMsec) =
        Handy.timeMsec:
          Partitions.count(n)
        println(pad(n) + pad(s"${timeMsec}ms") + s"$count")

  val (min, sec) = (totalTimeMsec/1000) /% 60
  println(s"total time: $min min $sec sec")
