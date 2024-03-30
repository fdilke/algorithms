package com.fdilke.backtrack.node

import munit.FunSuite

import com.fdilke.utility.RichFunSuite._


class NodeSpec extends FunSuite:
  test("successfully fails at the first hurdle"):
    class NonStarterNode extends Node[NonStarterNode, Unit]:
      override def explore: NodeStatus[NonStarterNode, Unit] =
        NodeBad
    NonStarterNode().solve is None

  test("successfully succeeds at the first hurdle"):
    class QuickWinNode extends Node[QuickWinNode, Int]:
      override def explore: NodeStatus[QuickWinNode, Int] =
        NodeGood(2)
    val node = QuickWinNode()
    node.solve is Some(2)

  test("successfully increments a value to 5"):
    class SearchNode(i: Int) extends Node[SearchNode, Boolean]:
      override def explore: NodeStatus[SearchNode, Boolean] =
        if (i == 5)
          NodeGood(true)
        else
          NodeContinue(Iterable(SearchNode(i + 1)))
    val initialNode = SearchNode(0)
    initialNode.solve is Some(true)

