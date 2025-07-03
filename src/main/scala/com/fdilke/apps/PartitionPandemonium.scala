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
        m <- 0 to 100
        n = 100 * m
      do
        val (count, timeMsec) =
        Handy.timeMsec:
          Partitions.slowCount(n)
        println(pad(n) + pad(s"${timeMsec}ms") + s"$count")

  val (min, sec) = (totalTimeMsec/1000) /% 60
  println(s"total time: $min min $sec sec")

object PartitionPandemonium2 extends App:
  def pad(x: String): String =
    x.padTo(10, ' ')
  def pad(x: Number): String =
    pad(x.toString)

  val maxModulus = 300
  val residues: Array[Array[Boolean]] =
    Array.tabulate(maxModulus+1): i =>
      Array.tabulate(i): j =>
        false

  val (_, totalTimeMsec) =
    Handy.timeMsec:
      for
        n <- 1 to 3000
      do
        val (count, timeMsec) =
          Handy.timeMsec:
            Partitions.slowCount(n)
        for
          i <- 1 to maxModulus
        do
          residues(i)(count.mod(i).toInt) = true

        println(pad(n) + pad(s"${timeMsec}ms") + s"$count")

  val (min, sec) = (totalTimeMsec/1000) /% 60
  println(s"total time: $min min $sec sec")

  for
    i <- 1 to maxModulus
  do
    residues(i).zipWithIndex.find:
      (value, i) => !value
    match
      case None =>
      case Some((_, r)) =>
        println(s"residue missing: $r mod $i")

