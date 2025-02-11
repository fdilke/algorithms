package com.fdilke.backtrack

import cats.Monad
import com.fdilke.backtrack.node.{MonadIterable, Node}

import scala.collection.mutable

object Backtrack extends BacktrackSolver:
  def apply[
    NODE,
    F[_] : Monad,
    SOLUTION
  ](
     startNode: NODE
   )(
   explore: NODE => F[Either[NODE, SOLUTION]]
  ): F[SOLUTION] =
    Monad[F].tailRecM[NODE, SOLUTION](startNode):
      explore

  val dedup: BacktrackSolver =
    new BacktrackSolver:
      def apply[
        NODE,
        F[_] : Monad,
        SOLUTION
      ](
        startNode: NODE
      )(
       explore: NODE => F[Either[NODE, SOLUTION]]
      ): F[SOLUTION] =
        type CHOICE = Either[NODE, SOLUTION]
        lazy val emptyChoice: F[CHOICE] =
          if (Monad[F] == Monad[Iterable])
            Iterable.empty[CHOICE].asInstanceOf[F[CHOICE]]
          else
            throw new IllegalArgumentException("unknown Monad[F]")

        def isSeen(
          node: NODE,
          seenNodes: mutable.ListBuffer[NODE]
        ): Boolean =
          if (seenNodes.contains(node))
            true
          else
            seenNodes += node
            false
        
        val seenNodesGenerator: F[() => mutable.ListBuffer[NODE]] =
          Monad[F].pure:
            () => mutable.ListBuffer.empty[NODE]

        Monad[F].flatMap(seenNodesGenerator):
          generatorF =>
          val seenNodes = generatorF()
          Monad[F].tailRecM[NODE, SOLUTION](startNode): node =>
            if (isSeen(node, seenNodes))
              emptyChoice
            else
              explore(node)

trait BacktrackSolver:
  def apply[
    NODE,
    F[_] : Monad,
    SOLUTION
  ](
     startNode: NODE
   )(
   explore: NODE => F[Either[NODE, SOLUTION]]
  ): F[SOLUTION]
