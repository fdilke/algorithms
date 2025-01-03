package com.fdilke.utility

import munit.{ FunSuite, Assertions }
import Assertions.{ assertEquals, assertNotEquals }

object RichFunSuite:
  extension[A] (a: A)
    inline infix def is(b: A): Unit =
      assertEquals(a, b)
    inline infix def isnt(b: A): Unit =
      assertNotEquals(a, b)

  extension(letters: Seq[Char])
    inline def be(text: String): Unit =
      assertEquals(letters.mkString, text)

  def checkSameElementsAs[X](thing: Seq[X], other: Seq[X]): Unit =
    thing.size is other.size
    thing.toSet is other.toSet



