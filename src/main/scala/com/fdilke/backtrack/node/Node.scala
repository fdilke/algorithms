package com.fdilke.backtrack.node


trait Node[+NODE <: Node[NODE, SOLUTION], +SOLUTION]:
  node =>
  def explore: NodeStatus[NODE, SOLUTION]
  final def solve: Option[SOLUTION] =
    explore match
      case NodeBad => None
      case NodeGood(solution) => Some(solution)

sealed trait NodeStatus[+NODE <: Node[NODE, SOLUTION], +SOLUTION]

case class NodeGood[+SOLUTION](solution: SOLUTION) extends NodeStatus[Nothing, SOLUTION]
case object NodeBad extends NodeStatus[Nothing, Nothing]


