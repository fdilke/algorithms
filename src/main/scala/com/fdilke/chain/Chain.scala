package com.fdilke.chain

import scala.annotation.tailrec

sealed trait Chain[T]:
  def step: Either[T, Chain[T]]
  final def run()(implicit runner: ChainRunner): T =
    runner.run(this)
  
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
