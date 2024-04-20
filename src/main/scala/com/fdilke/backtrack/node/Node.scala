package com.fdilke.backtrack.node

trait Node[SOLUTION]:
  type NodeChoice = Either[Node[SOLUTION], SOLUTION]
  type NodeStatus = Iterable[NodeChoice]
  final def solution(s: SOLUTION): NodeChoice =
    Right[Node[SOLUTION], SOLUTION](s)
  final def node(n: Node[SOLUTION]): NodeChoice =
    Left[Node[SOLUTION], SOLUTION](n)
  def explore: NodeStatus

trait NodeSolver:
  def allSolutions[SOLUTION](
    node: Node[SOLUTION]
  ): Iterable[SOLUTION]

  final def oneSolution[SOLUTION](
    node: Node[SOLUTION]
  ): Option[SOLUTION] =
    allSolutions(node).headOption

