package com.fdilke.backtrack.node

import cats.Monad
import cats.free.Free
import Free.liftF

object NodeSolvers:
  
  object NaiveNodeSolver extends NodeSolver:
    override def allSolutions[F[_] : Monad, SOLUTION](
      node: Node[F, SOLUTION]
    ): F[SOLUTION] =
      val monad: Monad[F] = implicitly
      monad.flatMap(node.explore) {
        case Right(solution) => monad.pure(solution)
        case Left(node) => allSolutions(node)
      }

  object StackSafeNodeSolver extends NodeSolver:
    override def allSolutions[F[_] : Monad, SOLUTION](
      node: Node[F, SOLUTION]
    ): F[SOLUTION] =
      Monad[F].tailRecM[Node[F, SOLUTION], SOLUTION](node):
        _.explore

  object FancyFreeNodeSolver extends NodeSolver:
    override def allSolutions[F[_] : Monad, SOLUTION](
      node: Node[F, SOLUTION]
    ): F[SOLUTION] =
      type NodeF[S] = Node[F, S]
      type FreeNode[A] = Free[NodeF, A]
      def pure[S](s: S): FreeNode[S] =
        liftF(Node.pure[F, S](s))
      def defer[S](next: F[Either[Node[F, S], S]]): FreeNode[S] =
        liftF(
          new NodeF[S]:
            override def explore: NodeStatus =
              next
        )
      ???



