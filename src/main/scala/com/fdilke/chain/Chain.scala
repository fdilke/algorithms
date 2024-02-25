package com.fdilke.chain

import scala.annotation.tailrec

sealed trait Chain[T]:
  def step: Either[T, Chain[T]]
  final def run()(implicit runner: ChainRunner): T =
    runner.run(this)
  def map[U](function: T => U): Chain[U]
  
object Chain:
  object VanillaRunner extends ChainRunner:
    @tailrec override def run[T](chain: Chain[T]): T =
      chain.step match
        case Left(result) => result
        case Right(nextChain) => run(nextChain)

  def pure[T](result: T): Chain[T] =
    PureChain[T](result)

trait ChainRunner:
  def run[T](chain: Chain[T]): T

private class PureChain[T](result: T) extends Chain[T]:
  override def step: Either[T, Chain[T]] =
    Left(result)
  override def map[U](function: T => U): Chain[U] =
    MappedChain[T, U](this, function)

private class UnitChain[T](result: => T) extends Chain[T]:
  override def step: Either[T, Chain[T]] =
    Left(result)
  override def map[U](function: T => U): Chain[U] =
    MappedChain[T, U](this, function)

private class MappedChain[T, U](
  link: Chain[T],
  function: T => U
) extends Chain[U]:
  override def step: Either[U, Chain[U]] =
    link.step match
    case Left(result) => Right(UnitChain { function(result) })
    case Right(nextChain) => Right(MappedChain(nextChain, function))

  override def map[V](function: U => V): Chain[V] =
    MappedChain[U, V](this, function)


