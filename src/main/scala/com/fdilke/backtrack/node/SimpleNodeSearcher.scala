package com.fdilke.backtrack.node

class SimpleNodeSearcher extends NodeSolver:
  override def allSolutions[SOLUTION](node: Node[SOLUTION]): Iterable[SOLUTION] =
    node.explore match
      case NodeBad => None
      case NodeGood(solution) => Iterable(solution)
      case NodeContinue(nextNodes) =>
        nextNodes.view flatMap allSolutions


