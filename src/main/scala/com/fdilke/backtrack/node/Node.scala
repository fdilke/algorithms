package com.fdilke.backtrack.node


trait Node[+NODE <: Node[NODE]]:
  def explore: NodeStatus[NODE]
  final def solve: Option[NODE] =
    val pig: NodeStatus[NODE] = NodeFailed
    explore match {
      case NodeFailed => None
    }

// object NodeSucceeded extends NodeStatus[_]
sealed trait NodeStatus[+NODE <: Node[NODE]]

case object NodeFailed extends NodeStatus[Nothing]


