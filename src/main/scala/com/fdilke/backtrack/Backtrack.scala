package com.fdilke.backtrack

import cats.Monad
import com.fdilke.backtrack.node.{MonadIterable, Node}

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

        var seenNodes: List[NODE] = Nil
        def isSeen(node: NODE): Boolean =
          if (seenNodes.contains(node))
            true
          else
            seenNodes = node +: seenNodes
            false

        Monad[F].tailRecM[NODE, SOLUTION](startNode): node =>
          if (isSeen(node))
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
