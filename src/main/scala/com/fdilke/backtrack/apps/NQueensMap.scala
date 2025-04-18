package com.fdilke.backtrack.apps

import com.fdilke.backtrack.MapBacktrack
import com.fdilke.backtrack.MapBacktrack.{DecisionNode, MapComplete, MapContinue, MapInvalid, NextStep}

// using the old 'MapBacktrack' method
object NQueensMap extends App {
  val order = 6

  val columns: Set[Int] = (0 until order).toSet
  val coordinates: Set[(Int, Int)] =
    for {
      x <- columns
      y <- columns
    } yield
      (x, y)

  def lastOneAttacks(
    map: Map[Int, (Int, Int)]
  ): Boolean = {
    val last = map.keySet.max
    val (lastX, lastY) = map(last)

    (0 until last).exists { i =>
      val (x, y) = map(i)

      (x == lastX) ||
      (y == lastY) ||
      (  (x - lastX) == (y - lastY) ) ||
      (  (lastX - x) == (y - lastY) )
    }
  }

  lazy val queensNode: DecisionNode[Int, (Int, Int)] =
    map =>
      if map.isEmpty then
        MapContinue(0, queensNode)
      else if lastOneAttacks(map) then
        MapInvalid.asInstanceOf[NextStep[Int, (Int, Int)]]
      else if map.size == order then
        MapComplete.asInstanceOf[NextStep[Int, (Int, Int)]]
      else
        MapContinue(map.keySet.max + 1, queensNode)

  def showMap(
    map: Map[Int, (Int, Int)]
  ): Unit = {
    val queens: Set[(Int, Int)] = map.values.toSet
    println("-----------------")
    for { i <- columns } {
      for { j <- columns } {
        if (queens.contains( i -> j ))
          print("Q")
        else
          print("-")
      }
      println()
    }
  }

  val solutions: Iterable[Map[Int, (Int, Int)]] =
    MapBacktrack.solve[Int, (Int, Int)](
      coordinates,
      queensNode
    )
  if false then
    solutions foreach showMap
  else
    println(s"${solutions.size} solutions")
}
