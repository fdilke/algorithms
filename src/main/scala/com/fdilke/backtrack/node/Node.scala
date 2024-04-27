package com.fdilke.backtrack.node

import cats.Monad

trait NodeIterable[SOLUTION] extends Node[Iterable, SOLUTION]

trait Node[F[_], SOLUTION]:
  protected type NodeChoice = Either[Node[F, SOLUTION], SOLUTION]
  protected type NodeStatus = F[NodeChoice]
  final def solution(s: SOLUTION): NodeChoice =
    Right[Node[F, SOLUTION], SOLUTION](s)
  final def node(n: Node[F, SOLUTION]): NodeChoice =
    Left[Node[F, SOLUTION], SOLUTION](n)
  def explore: NodeStatus

trait NodeSolver:
  def allSolutions[F[_] : Monad, SOLUTION](
    node: Node[F ,SOLUTION]
  ): F[SOLUTION]

// TODO: complete steps to making it into a monad. Make it into NodeMonad

object Node:
  def pure[F[_] : Monad, SOLUTION](
    s: SOLUTION
  ): Node[F, SOLUTION] =
    new Node[F, SOLUTION]:
      override def explore: NodeStatus =
        Monad[F].pure(
          solution(s)
        )
  def map[F[_]: Monad, A, B](
    nodeA: Node[F, A]
  )(
    f: A => B
  ): Node[F, B] =
    new Node[F, B]:
      override def explore: NodeStatus =
        Monad[F].map(nodeA.explore):
          _ match {
            case Left(na) =>
              Left(map[F, A, B](na)(f))
            case Right(a) =>
              Right(f(a))
          }
