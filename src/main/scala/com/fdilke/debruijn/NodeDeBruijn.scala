package com.fdilke.debruijn

import com.fdilke.backtrack.node.{GenericNode, NodeIterable}
import com.fdilke.utility.SetsUtilities.*
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver
import com.fdilke.backtrack.node.MonadIterable.*

object NodeDeBruijn extends DeBruijn:
  def apply(n: Int, k: Int): Seq[Int] =
    val n_k = naivePow(n, k)
    type NTuple = Seq[Int]
    case class OpCycle(
                        operation: Map[NTuple, Int],
                        cycle: Seq[Int]
                      ) extends NodeIterable[OpCycle]:
      override def explore: NodeStatus =
        val wipLength = cycle.length
        val endTuple = cycle.slice(wipLength - n, wipLength)
        if (cycle.length == n_k)
          println(s"n =$n, k = $k, cycle length: ${wipLength}, operations = ${operation.size}")
          if (operation(endTuple) == cycle(0))
            Iterable(solution(this))
          else
            Iterable()
        else
          if (operation.get(endTuple).isDefined)
            Iterable()
          else
            val possibles: Iterable[Int] = 0 until k
            possibles map:
              i =>
                solution(OpCycle(
                  operation + (endTuple -> i),
                  cycle :+ i
                ))
    StackSafeNodeSolver.allSolutions[GenericNode, Iterable, OpCycle](
      OpCycle(
        Map.empty,
        Seq.fill(n)(0)
      )
    ).head.cycle

