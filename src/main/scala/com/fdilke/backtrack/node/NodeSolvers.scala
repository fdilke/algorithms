package com.fdilke.backtrack.node

import cats.Monad

object NodeSolvers:
  
  class NaiveNodeSolver extends NodeSolver:
    override def allSolutions[F[_] : Monad, SOLUTION](
      node: Node[F, SOLUTION]
    ): F[SOLUTION] =
      val monad: Monad[F] = implicitly
      monad.flatMap(node.explore) {
        case Right(solution) => monad.pure(solution)
        case Left(node) => allSolutions(node)
      }

  class StackSafeNodeSolver extends NodeSolver:
    override def allSolutions[F[_] : Monad, SOLUTION](
      node: Node[F, SOLUTION]
    ): F[SOLUTION] =
      val monad: Monad[F] = implicitly
      monad.tailRecM[Node[F, SOLUTION], SOLUTION](node):
        _.explore


