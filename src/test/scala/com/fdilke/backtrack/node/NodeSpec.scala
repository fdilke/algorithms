package com.fdilke.backtrack.node

import munit.FunSuite

import com.fdilke.utility.RichFunSuite._


class NodeSpec extends FunSuite:
  test("successfully fails at the first hurdle"):
    class NonStarterNode extends Node[NonStarterNode]:
      override def explore: NodeStatus[NonStarterNode] =
        NodeFailed

    NonStarterNode().solve is None

