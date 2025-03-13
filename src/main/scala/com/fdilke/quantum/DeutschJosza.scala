package com.fdilke.quantum

import org.redfx.strange.{Complex, Program, QuantumExecutionEnvironment, Qubit, Result, Step}

import scala.util.Random
import org.redfx.strange.local.SimpleQuantumExecutionEnvironment
import org.redfx.strange.gate.{Hadamard, Oracle, X}

object DeutschJosza extends App:
  deutschJosza(3)

  private def deutschJosza(N: Int): Unit =
    val simulator: QuantumExecutionEnvironment =
      SimpleQuantumExecutionEnvironment()
    val random: Random =
      Random()
    var program: Program =
      null
    for (i <- 0 until 10) do
      program = new Program(N + 1)
      val step0 = Step()
      step0.addGate(X(N))

      val step1 = new Step()
      for j <- 0 until N + 1 do
        step1.addGate(new Hadamard(j))

      val step2 = new Step()
      val choice = 1
        // random.nextInt(2)
      val oracle = createOracle(N, choice)
      step2.addGate(oracle)

      val step3 = new Step()
      for j <- 0 until N do
        step3.addGate(Hadamard(j))

      program.addStep(step0)
      program.addStep(step1)
      program.addStep(step2)
      program.addStep(step3)
      val result: Result =
        simulator.runProgram(program)
      val qubits: Array[Qubit] =
        result.getQubits
      println("f = " + choice + ", val = " + qubits(0).measure())
    // Renderer.renderProgram(program)

  private def createOracle(N: Int, f: Int): Oracle =
    val dim = 2 << N
    val half = dim / 2

    val matrix: Array[Array[Complex]] =
      Array.fill[Array[Complex]](dim):
        Array.fill[Complex](dim):
          Complex.ZERO

    f match
      case 0 =>
        for i <- 0 until dim do
          matrix(i)(i) = Complex.ONE
        new Oracle(matrix)
      case 1 =>
        for i <- 0 until dim do
          if i % 2 == 0 then
            matrix(i)(i) = Complex.ONE
          else
            if i < half then
              matrix(i)(i + half) = Complex.ONE
            else
              matrix(i)(i - half) = Complex.ONE
        new Oracle(matrix);
      case _ =>
        throw new IllegalArgumentException("Wrong index in oracle")
