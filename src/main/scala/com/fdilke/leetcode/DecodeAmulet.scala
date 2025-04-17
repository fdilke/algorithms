package com.fdilke.leetcode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// Based on an interview test question
// We're given a list of pairs of symbols, representing randomly ordered
// edges around the circumference of a mysterious amulet. Return a cycle.

object DecodeAmulet:
  def apply[A](
    pairs: (A, A)*
  ): Seq[A] =
    if pairs.isEmpty then
      Seq.empty
    else
      val symbols: Seq[A] =
        pairs.flatMap:
          (x, y) => Seq(x, y)
        .distinct
      val pairMaps: mutable.Map[A, Seq[A]] =
        mutable.Map[A, Seq[A]](
          (
            symbols.map: a =>
              a -> Seq.empty[A]
          )*
        )
      def processPair(x: A, y: A): Unit =
        pairMaps.updateWith(x):
          _.map:
            _ :+ y
      for
        (x, y) <- pairs
      do
        if x == y then
          throw IllegalArgumentException("contains a loop")
        processPair(x, y)
        processPair(y, x)
      var nextSymbol: A =
        pairs.head._1
      val cycle: ArrayBuffer[A] =
        ArrayBuffer[A](nextSymbol)
      var running: Boolean = true
      while running do
        pairMaps(nextSymbol).find:
          !cycle.contains(_)
        match
          case None =>
            running = false
          case Some(a) =>
            cycle.append(a)
            nextSymbol = a
      if cycle.size == symbols.size &&
        pairMaps(cycle.last).contains(cycle.head)
      then
        cycle.toSeq
      else
        throw IllegalArgumentException("faulty cycle structure")

