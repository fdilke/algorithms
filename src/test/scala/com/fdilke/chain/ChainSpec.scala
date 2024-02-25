package com.fdilke.chain

import munit.FunSuite
import com.fdilke.utility.RichFunSuite._

class ChainSpec extends FunSuite:
  given ChainRunner = Chain.VanillaRunner

  test("can create and run a pure chain of 2"):
    Chain.pure(2).run() is 2

  test("can map a chain"):
    val mappedChain: Chain[String] = Chain.pure(2).map { _.toString }
    mappedChain.run() is "2"

  test("can flatMap a chain"):
    def kleisli(text: String): Chain[Int] =
      Chain.pure(text).map { _.length }
    val flatMappedChain: Chain[Int] = Chain.pure("David Sylvian").flatMap(kleisli)
    flatMappedChain.run() is 13


