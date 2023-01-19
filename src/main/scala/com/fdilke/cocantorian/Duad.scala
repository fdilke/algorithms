package com.fdilke.cocantorian

import scala.Function.tupled
import scala.annotation.tailrec
import scala.language.postfixOps

object Duad {
/*
  @tailrec
  def isPowerOf2(n: Int): Boolean =
    if (n < 1)
      false
    else if (n == 1)
      true
    else if (n % 2 == 1)
      false
    else
      isPowerOf2(n / 2)

  @inline def canonical[
    H: Hausdorff
  ](
    Duad: H*
  ): (Seq[H], Int) =
    canonicalSub(Duad, Duad.length)

  @tailrec private def canonicalSub[
    H: Hausdorff
  ](
    Duad: Seq[H],
    len: Int
  ): (Seq[H], Int) =
    if (len == 1)
      (Duad, 1)
    else {
      val len_2 = len / 2
      val front = Duad.take(len_2)
      val rear = Duad.slice(len_2, len)
      if (front.zip(rear).forall {
            tupled(Hausdorff[H].equalH)
          })
        canonicalSub(front, len_2)
      else
        (Duad, len)
    }

  def apply[
    H: Hausdorff
  ](
    cycle: H*
  ): Duad[H] =
    if (!isPowerOf2(cycle.length))
      throw new IllegalArgumentException(s"Cycle length ${cycle.length} is not a power of 2")
    else {
      val (canonicalCycle, length): (Seq[H], Int) =
        canonical(cycle: _*)
      new Duad(
        canonicalCycle,
        length
      )
    }

  def η[H: Hausdorff](t: H): Duad[H] =
    Duad(t)

  def μ[H: Hausdorff](
    dd: Duad[
      Duad[H]
    ]
  ): Duad[H] =
    Duad(
      (0 until Math.max(
        dd.length,
        dd.cycle.map {
          _.length
        } max
      )).map(index => dd(index)(index)): _*
    )

  implicit def jonssonTarski[
    H: Hausdorff
  ]: JonssonTarski[Duad[H]] =
    new JonssonTarski[Duad[H]] {
      override def join(
        l: Duad[H],
        r: Duad[H]
      ): Duad[H] =
        Duad.join(l, r)

      override def left(
        Duad: Duad[H]
      ): Duad[H] =
        Duad.left

      override def right(
        Duad: Duad[H]
      ): Duad[H] =
        Duad.right
    }

  implicit def hausdorffDuad[
    H: Hausdorff
  ]: Hausdorff[Duad[H]] =
    Catcher.hausdorff[Duad[H], Boolean, H]

  implicit def catcherDuad[
    H: Hausdorff
  ]: Catcher[Duad[H], Boolean, H] =
    new Catcher[Duad[H], Boolean, H] {
      override def either(
        Duad: Duad[H]
      ): Either[H, Boolean => Duad[H]] =
        if (Duad.length == 1)
          Left(Duad(0))
        else
          Right {
            if (_)
              Duad.right
            else
              Duad.left
          }

      override def construct(
        e: => Either[
          H,
          Boolean => Duad[H]
        ]
      ): Duad[H] = e match {
        case Left(h) => Duad(h)
        case Right(bool2Duad) =>
          Duad.join(
            bool2Duad(false),
            bool2Duad(true)
          )
      }
    }

  def join[H: Hausdorff](
    l: Duad[H],
    r: Duad[H]
  ): Duad[H] =
    Duad(
      Seq.concat(
        (0 until Math.max(l.length, r.length)).map { index =>
          Seq(
            l(index),
            r(index)
          )
        }: _*
      ): _*
    )
}

class Duad[H: Hausdorff] private (
  val cycle: Seq[H],
  val length: Int
) {
  override def hashCode(): Int =
    Hausdorff.intKey(this)

  override def equals(obj: Any): Boolean =
    Hausdorff.equalH(
      obj.asInstanceOf[Duad[H]],
      this
    )

  def map[
    K: Hausdorff
  ](
    f: H => K
  ): Duad[K] =
    Duad(
      cycle.map(f): _*
    )

  def apply(index: Int): H =
    cycle(
      if (index >= 0)
        index % length
      else
        length + (index % length)
    )

  def apply[P](p: P)(
    implicit pitcherTude: Pitcher[P, Boolean]
  ): H =
    applyCatcher(this)(p)

  def flatMap[
    K: Hausdorff
  ](
    f: H => Duad[K]
  ): Duad[K] =
    μ[K](map(f))

  override def toString: String =
    "Duad(" + cycle.mkString(",") + ")"

  def left: Duad[H] =
    Duad(
      Range(0, cycle.length, 2).map {
        apply
      }: _*
    )

  def right: Duad[H] =
    Duad(
      Range(1, cycle.length + 1, 2).map {
        apply
      }: _*
    )
*/    
}
