package com.fdilke.offcuts

import cats.{Id, StackSafeMonad}
import com.fdilke.utility.Handy

import scala.annotation.tailrec

object OffCuts:

  object HackStack extends App:
    def corecurse(depth: Int): Unit =
      recurse(depth)

    def recurse(depth: Int): Unit =
      if depth == 40 then
        println("Hit max depth")
        val trace: Seq[StackTraceElement] =
          Thread.currentThread().getStackTrace.toSeq
        println("size = " + trace.size)
        trace.foreach { tel =>
          println("tel = " + tel.toString)
        }
      else
        corecurse(depth + 1)

    recurse(0)

object UnsafeMonads extends App:
  object UnsafeMonadIterable extends StackSafeMonad[Id]:
    override def pure[A](x: A): A =
      x
    override def flatMap[A, B](fa: A)(f: A => B): B =
      f(fa)
  def testLevel(maxLevel: Int): Int =
    UnsafeMonadIterable.tailRecM(0):
      level =>
      if level == maxLevel then
        Left(level + 1)
      else
        Right(Handy.stackDepth())
  println("testLevel(5) = " + testLevel(5))
  println("testLevel(10) = " + testLevel(10))

object SnailRecursion extends App:
  def snailRecM[A, B](a: A)(f: A => Seq[Either[A, B]]): Seq[B] =
    f(a) flatMap:
      case Left(a) => snailRecM(a)(f)
      case Right(b) => Seq(b)
  def testLevel(maxLevel: Int): Int =
    snailRecM(0):
      level => Seq(
        if level == maxLevel then
          Left(level + 1)
        else
          Right(Handy.stackDepth())
      )
    .head
  Seq(5, 10, 100, 1000).foreach:
    k =>
    println(s"testLevel(k) = ${ testLevel(k) }")
  def failRec[X](n: Int, start: X)(f: X => X): X =
    if (n == 0) then
      start
    else
      f(failRec(n - 1, start)(f))
  def testFail(level: Int) =
    failRec[Int](level, level) { i =>
      if i > 1 then
        i - 1
      else
        Handy.stackDepth()
    }
  println()
  Seq(5, 10, 20).foreach:
    k =>
      println(s"testFail(k) = ${ testFail(k) }")
  def failFibo(n: Int): Int =
    var maxDepth: Int = 0
    def failFiboR(n: Int): Int =
      n match
        case 0 =>
          maxDepth = Math.max(maxDepth, Handy.stackDepth())
          0
        case 1 => 1
        case m => failFiboR(m - 1) + failFiboR(m - 2)
    failFiboR(n)
    maxDepth
  println()
  Seq(5, 10, 20).foreach:
    k =>
    println(s"failFibo(k) = ${failFibo(k)}")
