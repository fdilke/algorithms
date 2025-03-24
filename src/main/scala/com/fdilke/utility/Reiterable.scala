package com.fdilke.utility

import java.util.concurrent.atomic.AtomicReference
import java.util.function.{Predicate, UnaryOperator}

class Reiterable[T](
  val initial: T,
  val nextOperator: UnaryOperator[T],
  val continuePredicate: Predicate[T]
) extends Iterable[T]:
  override def iterator: Iterator[T] =
    new Iterator[T]:
      var times: Int = 0
      private var current: T =
        initial
//      private val current: AtomicReference[T] =
//        AtomicReference(initial)
      override def hasNext: Boolean =
        println(s"$times) hasNext: current = $current")
        val test = continuePredicate.test(current)
        println(s"test = $test")
        times += 1
        println(s"times = $times")
        if times > 20 then
          throw new IllegalArgumentException("bur de bur")
        test
      override val next: T =
        println(s"$times) next: current = $current")
//        val thing = current.getAndUpdate(nextOperator)
        val thing = current
        current = nextOperator.apply(thing)
        println(s"thing = $thing")
        times += 1
        println(s"times = $times")
        if times > 20 then
          throw new IllegalArgumentException("bur de bur")
        thing
//      override def hasNext: Boolean =
//        continuePredicate.test(current.get)
//      override val next: T =
//        current.getAndUpdate(nextOperator)

object DunceSpike extends App:
  val holder = AtomicReference[Int](5)
  println(s"holder = ${holder.get()}")
  val value = holder.getAndUpdate(_ + 1)
  println(s"value = $value")
  println(s"holder = ${holder.get()}")

class ReiterableLite[T](
 val initial: T,
 val nextOperator: UnaryOperator[T],
 val continuePredicate: Predicate[T]
) extends Iterable[T]:
    override def iterator: Iterator[T] =
      new Iterator[T]:
        var times: Int = 0
        private var current: T =
          initial

        //      private val current: AtomicReference[T] =
        //        AtomicReference(initial)
        override def hasNext: Boolean =
          println(s"$times) hasNext: current = $current")
          val test = continuePredicate.test(current)
          println(s"test = $test")
          times += 1
          println(s"times = $times")
          if times > 20 then
            throw new IllegalArgumentException("bur de bur")
          test

        override val next: T =
          println(s"$times) next: current = $current")
          //        val thing = current.getAndUpdate(nextOperator)
          val thing = current
          current = nextOperator.apply(thing)
          println(s"thing = $thing")
          times += 1
          println(s"times = $times")
          if times > 20 then
            throw new IllegalArgumentException("bur de bur")
          thing

class ReiterableLite2[T](
  initial:T,
  nextOperator: UnaryOperator[T],
  continuePredicate: Predicate[T]
) extends Iterable[T]:
      override def iterator: Iterator[T] =
        new Iterator[T]:
          var value: T = initial
          override def hasNext: Boolean =
            continuePredicate.test(value)
          override def next: T =
            val retVal = value
            value = nextOperator(value)
            retVal

//      override def hasNext: Boolean =
object NoddierDunceSpike extends App:
  val initial: Int = 0
  val theF: Int => Int =
    _ + 1
  val condition: Int => Boolean =
    _ < 5
  val itt: Iterable[Int] =
    new ReiterableLite2[Int](
      initial,
      theF(_),
      condition(_)
    )
//    new Iterable[Int]:
//      override def iterator: Iterator[Int] =
//        new Iterator[Int]:
//          var value: Int = initial
//          override def hasNext: Boolean =
//            condition(value)
//          override def next: Int =
//            val retVal = value
//            value = theF(value)
//            retVal
  println(s"itt = $itt")
  val comparison: Boolean =
    itt.toSeq == Seq(0,1,2,3,4)
  println(s"comparison = $comparison")
