package com.fdilke.debruijn

import com.fdilke.backtrack.node.NodeIterable
import com.fdilke.utility.SetsUtilities.*
import com.fdilke.backtrack.node.NodeSolvers.StackSafeNodeSolver
import com.fdilke.backtrack.node.MonadIterable.*

object NodeDeBruijn extends DeBruijn:
    def apply(n: Int, k: Int): Seq[Int] =
        ???
        // val n_k = naivePow(n, k)
        // type SOLUTION = Array[Int]
        // class LocalNode(
        //     values: SOLUTION,
        //     position: Int
        // ) extends NodeIterable[SOLUTION]:
        //     override def explore: NodeStatus =
        //         if (position == n_k)
        //             Iterable(solution(values))
        //         else
        //             val possibles: Iterable[Int] = 0 until k
        //             possibles collect: 
        //                 i =>
        //                 val candidate: SOLUTION = values.clone()
        //                 candidate(position) = i
        //                 ...
                        
        //         Iterable(node(SearchNode(i + 1)))
        // StackSafeNodeSolver.allSolutions()
