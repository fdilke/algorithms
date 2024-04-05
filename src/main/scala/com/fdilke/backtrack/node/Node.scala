package com.fdilke.backtrack.node

trait Node[NODE <: Node[NODE, SOLUTION], SOLUTION]:
  node =>
  def explore: NodeStatus
  final def allSolutions: Iterable[SOLUTION] =
    explore match
      case NodeBad => None
      case NodeGood(solution) => Iterable(solution)
      case NodeContinue(nextNodes) => 
        nextNodes.view.map { 
          _.allSolutions 
        }.flatten

  final def solve: Option[SOLUTION] =
    allSolutions.headOption

  sealed trait NodeStatus

  case class NodeGood(solution: SOLUTION) extends NodeStatus
  case object NodeBad extends NodeStatus
  case class NodeContinue(
    nextNodes: Iterable[NODE]
  ) extends NodeStatus


