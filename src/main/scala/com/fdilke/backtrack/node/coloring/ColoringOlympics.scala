package com.fdilke.backtrack.node.coloring

import com.fdilke.truchet.TruchetGrid

import scala.util.Random

object ColoringOlympics extends App:
  val sampleWidth = 19
  val sampleHeight = 19
  val numIterations = 10000
  val sampleNumVertices = 60

  val truchetGenerator: () => Graph = () =>
      new TruchetGrid(
        width = sampleWidth,
        height = sampleHeight,
        toroidal = false,
        boolStream = Random(System.currentTimeMillis()),
        colorGenerator = TruchetGrid.colorGenerator,
        algo = NoEffortColoring
      ).regionGraph
  val rndPlanarGenerator: () => Graph = () =>
    Graph.randomPlanar(
      sampleNumVertices,
      Random(System.currentTimeMillis())
    )
  runIterations(rndPlanarGenerator)

  def runIterations(generator: () => Graph): Unit =
    val algos: Seq[(String, ColoringAlgo)] =
      Seq(
        "TweakLoop" -> ColorGraphTweakedLoop,
        "Loop" -> ColorGraphLoop,
        // "Joins" -> ColorGraphByJoins
      )
    val numAlgos: Int =
      algos.size
    val timings: Array[Long] =
      Array[Long](Seq.fill(numAlgos)(0)*)
    for (i <- 0 until numIterations) do
      print(".")
      val graph = generator()
      for (((_, algo), j) <- algos.zipWithIndex) do
        print("<")
        val startTime = System.currentTimeMillis()
        if algo(4, graph).isEmpty then
          throw new IllegalArgumentException("cannot color graph")
        val endTime = System.currentTimeMillis()
        val time = endTime - startTime
        timings(j) += time
        print(">")
      println(";")
    for (((name, _), j) <- algos.zipWithIndex) do
      println(s"$name\t${timings(j).toDouble / numIterations}ms")



