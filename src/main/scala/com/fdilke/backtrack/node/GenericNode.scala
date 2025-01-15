package com.fdilke.backtrack.node

import cats.Monad

trait NodeIterable[SOLUTION] extends GenericNode[Iterable, SOLUTION]

trait GenericNode[F[_], SOLUTION] extends Node[GenericNode, F, SOLUTION]

trait Node[NODE[F2[_], SOL2] <: Node[NODE, F2, SOL2], F[_], SOLUTION]:
  protected type NodeChoice = Either[NODE[F, SOLUTION], SOLUTION]
  protected type NodeStatus = F[NodeChoice]
  final def solution(s: SOLUTION): NodeChoice =
    Right[NODE[F, SOLUTION], SOLUTION](s)
  final def node(n: NODE[F, SOLUTION]): NodeChoice =
    Left[NODE[F, SOLUTION], SOLUTION](n)
  def explore: NodeStatus

trait NodeSolver:
  def allSolutions[
    NODE[F2[_], SOL2] <: Node[NODE, F2, SOL2], 
    F[_] : Monad, 
    SOLUTION
  ](
    node: NODE[F ,SOLUTION]
  ): F[SOLUTION]

// TODO: complete steps to making it into a monad. Make it into NodeMonad

object GenericNode:
  def pure[
    F[_] : Monad, 
    SOLUTION
  ](
    s: SOLUTION
  ): GenericNode[F, SOLUTION] =
    new GenericNode[F, SOLUTION]:
      override def explore: NodeStatus =
        Monad[F].pure(
          solution(s)
        )
  def map[F[_]: Monad, A, B](
    nodeA: GenericNode[F, A]
  )(
    f: A => B
  ): GenericNode[F, B] =
    new GenericNode[F, B]:
      override def explore: NodeStatus =
        Monad[F].map(nodeA.explore) {
          case Left(na) =>
            Left(map[F, A, B](na)(f))
          case Right(a) =>
            Right(f(a))
        }
