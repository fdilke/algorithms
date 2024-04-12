package com.fdilke.backtrack.node

trait Node[SOLUTION]:
  node =>
  protected type NodeStatus = NodeLocalStatus[SOLUTION]
  def explore: NodeStatus
  final def allSolutions: Iterable[SOLUTION] =
    explore match
      case NodeBad => None
      case NodeGood(solution) => Iterable(solution)
      case NodeContinue(nextNodes) =>
        nextNodes.view.flatMap(_.allSolutions)

  final def solve: Option[SOLUTION] =
    allSolutions.headOption

sealed trait NodeLocalStatus[+SOLUTION]

case class NodeGood[SOLUTION](solution: SOLUTION) extends NodeLocalStatus[SOLUTION]
case object NodeBad extends NodeLocalStatus[Nothing]
case class NodeContinue[SOLUTION](
  nextNodes: Iterable[Node[SOLUTION]]
) extends NodeLocalStatus[SOLUTION]


