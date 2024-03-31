package com.fdilke.backtrack.node

import munit.FunSuite

import com.fdilke.utility.RichFunSuite._


class NodeSpec extends FunSuite:
  test("successfully fails at the first hurdle"):
    class NonStarterNode extends Node[NonStarterNode, Unit]:
      override def explore: NodeStatus =
        NodeBad
    NonStarterNode().solve is None

  test("successfully succeeds at the first hurdle"):
    class QuickWinNode extends Node[QuickWinNode, Int]:
      override def explore: NodeStatus =
        NodeGood(2)
    val node = QuickWinNode()
    node.solve is Some(2)

  test("successfully increments a value to 5"):
    class SearchNode(i: Int) extends Node[SearchNode, Boolean]:
      override def explore: NodeStatus =
        if (i == 5)
          NodeGood(true)
        else
          NodeContinue(Iterable(SearchNode(i + 1)))
    val initialNode = SearchNode(0)
    initialNode.solve is Some(true)

  test("find a solution in a branching search"):
    val seqValues: Iterable[Boolean] = Iterable(true, false)
    class SearchNode(prefix: Seq[Boolean]) extends Node[SearchNode, Seq[Boolean]]:
      override def explore: NodeStatus =
        if (prefix.length == 3)
          NodeGood(prefix)
        else
          NodeContinue(
            seqValues.map { v => SearchNode(prefix :+ v) }
          )
    val initialNode = SearchNode(Seq.empty)
    initialNode.solve is Some(
      Seq(true, true, true)
    )

