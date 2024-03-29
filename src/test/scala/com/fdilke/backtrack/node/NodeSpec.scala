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

