package com.fdilke.backtrack.node

import cats.Monad

class SimpleNodeSearcher extends NodeSolver:
  override def allSolutions[F[_] : Monad, SOLUTION](
    node: Node[F, SOLUTION]
  ): F[SOLUTION] =
    val monad: Monad[F] = implicitly
    monad.flatMap(node.explore) {
      case Right(solution) => monad.pure(solution)
      case Left(node) => allSolutions(node)
    }


