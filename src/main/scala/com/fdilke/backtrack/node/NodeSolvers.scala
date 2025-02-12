package com.fdilke.backtrack.node

import cats.Monad

import scala.collection.mutable

object NodeSolvers:
  object NaiveNodeSolver extends NodeSolver:
    override def allSolutions[
      NODE <: Node[NODE, F, SOLUTION],
      F[_] : Monad,
      SOLUTION
    ](
      node: NODE
    ): F[SOLUTION] =
      val monad: Monad[F] = implicitly
      monad.flatMap(node.explore):
        case Right(solution) => monad.pure(solution)
        case Left(node) => allSolutions(node)

  object StackSafeNodeSolver extends NodeSolver:
    override def allSolutions[
      NODE <: Node[NODE, F, SOLUTION],
      F[_] : Monad,
      SOLUTION
    ](
      node: NODE
    ): F[SOLUTION] =
      Monad[F].tailRecM[NODE, SOLUTION](node):
        _.explore

  // Left in for now because I suspect the safe version is slower. Only used by the obsolete ColorGraphByJoins.  
  object StackSafeDedupNodeSolverBuggy extends NodeSolver:
    override def allSolutions[
      NODE <: Node[NODE, F, SOLUTION],
      F[_] : Monad,
      SOLUTION
    ](
      startNode: NODE
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
          node.explore

  // suspicions of slowness hang over this. See fast buggy version above, which doesn't return a reusable Iterable
  object StackSafeDedupNodeSolver extends NodeSolver:
    override def allSolutions[
      NODE <: Node[NODE, F, SOLUTION],
      F[_],
      SOLUTION
    ](
      startNode: NODE
    )(
      implicit monadF: Monad[F]
    ): F[SOLUTION] =
      type CHOICE = Either[NODE, SOLUTION]
      lazy val emptyChoice: F[CHOICE] =
        if (monadF == Monad[Iterable])
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

      monadF.flatMap(
        monadF.pure:
          () => mutable.ListBuffer.empty[NODE]
      ):
        generatorF =>
        val seenNodes = generatorF()
        Monad[F].tailRecM[NODE, SOLUTION](startNode): node =>
          if (isSeen(node, seenNodes))
            emptyChoice
          else
            node.explore

  /* A bold attempt, but I can't quite make the FreeT stuff work. Maybe need a simpler example to work from.
  object FancyFreeNodeSolver extends NodeSolver:
    override def allSolutions[
      NODE[F2[_], SOL2] <: Node[NODE, F2, SOL2],
      F[_] : Monad,
      SOLUTION
    ](
      node: NODE[F, SOLUTION]
    ): F[SOLUTION] =
      type NodeF[S] = NODE[F, S]
      type FreeNode[A] = Free[NodeF, A]
      def pure[S](s: S): FreeNode[S] =
        liftF(GenericNode.pure[F, S](s))
      def defer[S](
        next: F[Either[NODE[F, S], S]]
      ): FreeNode[S] =
        liftF(
          new NodeF[S]:
            override def explore: NodeStatus =
              next
        )
      def spikeCompiler: NodeF ~> F  =
        new (NodeF ~> F):
            override def apply[A](
              fa: NodeF[A]
            ): F[A] =
              ???
      def impureRun[S](prog: FreeNode[S]): F[S] = 
        prog.foldMap(spikeCompiler)
      ???

  object FancyFreeNodeSolver2 extends NodeSolver:
    override def allSolutions[F[_] : Monad, SOLUTION](
      node: Node[F, SOLUTION]
    ): F[SOLUTION] =
      type NodeF[S] = Node[F, S]
      type FreeNode[A] = FreeT[F, NodeF, A]
      // case class PureNodeStage[S](
      //   fs: F[S]
      // ) extends NodeS[S]
      // given Functor[NodeF] = new Functor[NodeF]:
      //   override def map[A, B](fa: NodeF[A])(f: A => B): NodeF[B] =
      //     Node.map[F, A, B](fa)(f)
      given Applicative[NodeF] = new Applicative[NodeF]:
        override def map[A, B](fa: NodeF[A])(f: A => B): NodeF[B] =
          Node.map[F, A, B](fa)(f)
      def pure1[S](ns: NodeF[S]): FreeNode[S] =
        FreeT.liftT[F, NodeF, S](ns)
      def pure2[S](fs: F[S]): FreeNode[S] =
        FreeT.liftF[F, NodeF, S](fs)
      def pure3[S](ff: F[FreeNode[S]]): FreeNode[S] =
        FreeT.roll[F, NodeF, S](ff)
      // case class DeferNodeStage[S](
      //   fn: F[Either[NodeS[S], S]]
      // ) extends NodeS[S]
      // def defer[S](
      //   next: F[Either[NodeS[S], S]]
      // ): FreeNode[S] =
      //   liftF(DeferNodeStage(next))
      def nodeCompiler: NodeF ~> F  =
        new (NodeF ~> F):
            override def apply[S](
              n: NodeF[S]
            ): F[S] =
              n match
                case PureNodeStage(fs) => fs
      def pureRun[S](
        prog: FreeNode[S]
      ): F[S] = 
        prog.foldMap(nodeCompiler)
      def nodeToFree[S](
        node: Node[F, S]
      ): NodeS[S] =
        Monad[F].map(node.explore) {
          case Left(n: Node[F, S]) => ???
          case Right(s: S) => ???
        }
      ???

  object FreeFireT:
    type S[A] = List[A]
    type M[A] = Option[A]
    type FreeNode[A] = FreeT[S, M, A]

    def pure1[A](ma: M[A]): FreeNode[A] =
      FreeT.liftT[S, M, A](ma)
    def pure2[A](sa: S[A]): FreeNode[A] =
      FreeT.liftF[S, M, A](sa)
    def pure3[A](sf: S[FreeNode[A]]): FreeNode[A] =
      FreeT.roll[S, M, A](sf)

    type T[A] = Iterable[A]
    def functionK: S ~> T  =
      new (S ~> T):
        override def apply[A](
          list: S[A]
        ): T[A] =
          list.headOption

    def remap: FunctionK[FreeNode, [A] =>> FreeT[T, M, A]] =
      FreeT.compile[S, T, M](functionK)
    def remap2: FreeNode ~> FreeT[T, M, *] =
      FreeT.compile[S, T, M](functionK)
    def bzzzt[S[_], T[_], M[_]: Functor](
      st: FunctionK[S, T]
    ): FunctionK[FreeT[S, M, _], FreeT[T, M, _]] =
      ???
    def bzzzt2[S[_], T[_], M[_]: Functor](
      st: FunctionK[S, T]
    ): FunctionK[FreeT[S, M, *], FreeT[T, M, *]] =
      ???
*/



