package com.fdilke.backtrack.node

class SimpleNodeSearcher extends NodeSolver:
  override def allSolutions[SOLUTION](node: Node[SOLUTION]): Iterable[SOLUTION] =
    node.explore.flatMap {
      case Right(solution) => Iterable(solution)
      case Left(node) => allSolutions(node)
    }

