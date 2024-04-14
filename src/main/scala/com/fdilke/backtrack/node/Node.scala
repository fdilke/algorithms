package com.fdilke.backtrack.node

trait Node[SOLUTION]:
  node =>
  protected type NodeStatus = NodeLocalStatus[SOLUTION]
  def explore: NodeStatus

sealed trait NodeLocalStatus[+SOLUTION]

case class NodeGood[SOLUTION](solution: SOLUTION) extends NodeLocalStatus[SOLUTION]
case object NodeBad extends NodeLocalStatus[Nothing]
case class NodeContinue[SOLUTION](
  nextNodes: Iterable[Node[SOLUTION]]
) extends NodeLocalStatus[SOLUTION]

trait NodeSolver:
  def allSolutions[SOLUTION](node: Node[SOLUTION]): Iterable[SOLUTION]

  final def oneSolution[SOLUTION](node: Node[SOLUTION]): Option[SOLUTION] =
    allSolutions(node).headOption

