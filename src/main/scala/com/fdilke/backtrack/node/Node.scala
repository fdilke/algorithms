package com.fdilke.backtrack.node

trait Node[SOLUTION]:
  type NodeStatus = Either[Iterable[SOLUTION], Iterable[Node[SOLUTION]]]
  def explore: NodeStatus

trait NodeSolver:
  def allSolutions[SOLUTION](
    node: Node[SOLUTION]
  ): Iterable[SOLUTION]

  final def oneSolution[SOLUTION](
    node: Node[SOLUTION]
  ): Option[SOLUTION] =
    allSolutions(node).headOption

