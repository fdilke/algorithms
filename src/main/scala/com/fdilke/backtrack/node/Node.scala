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
