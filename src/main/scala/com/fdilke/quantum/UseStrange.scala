package com.fdilke.quantum

import org.redfx.strange.gate.{Hadamard, X}
import org.redfx.strange.local.SimpleQuantumExecutionEnvironment
import org.redfx.strange.{Program, Qubit, Step}
import org.redfx.strangefx.render.Renderer

import scala.runtime.Arrays

object UseStrange extends App:
  val p: Program =
    Program(
      2,
      Step(
        X(0)
      ),
      Step(
        Hadamard(0), X(1)
      )
    )
  val sqee: SimpleQuantumExecutionEnvironment =
    SimpleQuantumExecutionEnvironment()
  val qubits: Array[Qubit]  =
    sqee.runProgram(p).getQubits
  Renderer.renderProgram(p)
  for q <- qubits do
    println(
      "qubit with probability on 1 = " + q.getProbability +
        ", measured it gives " + q.measure()
    )

