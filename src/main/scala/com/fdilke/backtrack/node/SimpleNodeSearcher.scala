package com.fdilke.backtrack.node

class SimpleNodeSearcher extends NodeSolver:
  override def allSolutions[SOLUTION](node: Node[SOLUTION]): Iterable[SOLUTION] =
    node.explore match
      case Left(solutions) => solutions
      case Right(nodes) =>
        nodes.view flatMap allSolutions

