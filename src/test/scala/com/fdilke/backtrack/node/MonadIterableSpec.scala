package com.fdilke.backtrack.node

import munit.FunSuite
import com.fdilke.utility.RichFunSuite.*

import java.util.concurrent.atomic.AtomicReference
import MonadIterable.*
import cats.Monad

class MonadIterableSpec extends FunSuite:
  private val monad: Monad[Iterable] = implicitly // check 'using'

  test("has pure"):
    val pure: Iterable[Int] = monad.pure[Int](3)
    pure.toSeq is Seq(3)

  test("has flatMap"):
    Iterable(0, 1).flatMap {
      case 0 => Iterable("xy", "z")
      case 1 => Iterable("w", "v", "k")
    }.toSeq is Seq("xy", "z", "w", "v", "k")

