package com.fdilke.backtrack.node

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*

import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import cats.Monad
import com.fdilke.utility.Handy.*

// implementing this myself because cats, and even alleycats, doesn't provide an implementation -
// (because Iterable is considered mutable and therefore unclean, or something)

class MonadIterableSpec extends FunSuite:
  private val monad: Monad[Iterable] = Monad[Iterable] // check 'using'

  test("has pure"):
    val pure: Iterable[Int] = monad.pure[Int](3)
    pure.toSeq is Seq(3)

  test("has map"):
    val mapped: Iterable[String] =
      monad.map[Int, String](
        Iterable[Int](3, 4)
      ){ _.toString }
    mapped.toSeq is Seq("3", "4")

  test("has flatMap"):
    monad.flatMap(Iterable(0, 1)) {
      case 0 => Iterable("xy", "z")
      case 1 => Iterable("w", "v", "k")
    }.toSeq is Seq("xy", "z", "w", "v", "k")

  test("has tailRecM, correctly calculated"):
    def reduceTo(b: Boolean): Iterable[Either[Int, Seq[Boolean]]] =
      Iterable(Right[Int, Seq[Boolean]](Seq(b)))
    def continue(is: Int*): Iterable[Either[Int, Seq[Boolean]]] =
      is map { i =>
        Left[Int, Seq[Boolean]](i)
      }
    val sampleInt: Int = 15
    monad.tailRecM[Int, Seq[Boolean]](sampleInt) {
      case 0 => reduceTo(false)
      case 1 => reduceTo(true)
      case i =>
        val half = i/2
        continue(half, i - half)
    }.toSeq is Seq.fill(sampleInt)(Seq(true))

  test("has tailRecM, using fixed stack size"):
    def testDepth(maxDepth: Int): Long =
      monad.tailRecM[Int, Long](0): (depth : Int) =>
        Iterable[Either[Int, Long]]:
          if depth < maxDepth then
            Left(depth + 1)
          else
            Right(stackDepth() : Long)
      .head
    val startDepth = stackDepth()
    testDepth(5) is testDepth(10)

  test("has tailRecM, adequately lazy"):
    val maxDepth = 3
    val invocations: AtomicInteger = AtomicInteger(0)
    class Bifurcate(
      val depth: Int
    ) extends Node[Bifurcate, Iterable, Int]:
      def explore: Iterable[Either[Bifurcate, Int]] =
        invocations.incrementAndGet()
        if depth < maxDepth then
          Iterable(
            node(Bifurcate(depth + 1)),
            node(Bifurcate(depth + 1))
          )
        else
          Iterable(solution(depth))
    val results: Iterable[(Int, Int)] =
      monad.tailRecM[Bifurcate, Int](Bifurcate(0)):
        _.explore
      .map: depth =>
        depth -> invocations.get()
    results.size is 8
    results.forall( _._1 == maxDepth ) is true
    results.map( _._2 ) is
      Seq(4, 5, 7, 8, 11, 12, 14, 15)
