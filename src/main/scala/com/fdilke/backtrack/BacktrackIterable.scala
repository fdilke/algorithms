package com.fdilke.backtrack

import cats.Monad
import com.fdilke.backtrack.node.{MonadIterable, Node}

import scala.collection.mutable

object BacktrackIterable extends BacktrackIterableSolver:
  def apply[
    NODE,
    SOLUTION
  ](
     startNode: NODE
   )(
      explore: NODE => Iterable[Either[NODE, SOLUTION]]
    ): Iterable[SOLUTION] =
    MonadIterable.tailRecM[NODE, SOLUTION](startNode):
      explore

  val dedup: BacktrackIterableSolver =
    new BacktrackIterableSolver:
      def apply[
        NODE,
        SOLUTION
      ](
        startNode: NODE
      )(
       explore: NODE => Iterable[Either[NODE, SOLUTION]]
      ): Iterable[SOLUTION] =
        new Iterable[SOLUTION]:
          override def iterator: Iterator[SOLUTION] =
            val seenNodes: mutable.ListBuffer[NODE] = 
              mutable.ListBuffer.empty[NODE]
            
            inline def isSeen(
              node: NODE
            ): Boolean =
              if (seenNodes.contains(node))
                true
              else
                seenNodes += node
                false
    
            MonadIterable.tailRecM[NODE, SOLUTION](startNode): node =>
              if (isSeen(node))
                Iterable.empty[Either[NODE, SOLUTION]]
              else
                explore(node)
            .iterator

  val dedupLegacy: BacktrackIterableSolver =
    new BacktrackIterableSolver:
      def apply[
        NODE,
        SOLUTION
      ](
        startNode: NODE
      )(
       explore: NODE => Iterable[Either[NODE, SOLUTION]]
      ): Iterable[SOLUTION] =
        def isSeen(
          node: NODE,
          seenNodes: mutable.ListBuffer[NODE]
        ): Boolean =
          if (seenNodes.contains(node))
            true
          else
            seenNodes += node
            false

        Iterable:
          () => mutable.ListBuffer.empty[NODE]
        .flatMap:
          generatorF =>
          val seenNodes = generatorF()
          MonadIterable.tailRecM[NODE, SOLUTION](startNode): node =>
            if (isSeen(node, seenNodes))
              Iterable.empty[Either[NODE, SOLUTION]]
            else
              explore(node)

trait BacktrackIterableSolver:
  def apply[
    NODE,
    SOLUTION
  ](
     startNode: NODE
   )(
   explore: NODE => Iterable[Either[NODE, SOLUTION]]
  ): Iterable[SOLUTION]
