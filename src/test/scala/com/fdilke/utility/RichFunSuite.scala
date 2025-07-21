package com.fdilke.utility

import munit.Assertions
import Assertions.{ assertEquals, assertNotEquals }

object RichFunSuite:
  extension[A] (a: A)
    inline infix def is(b: A): Unit =
      assertEquals(a, b)
    inline infix def isnt(b: A): Unit =
      assertNotEquals(a, b)

  extension[A] (a: Iterable[A])
    inline infix def isSet(b: Iterable[A]): Unit =
      assertEquals(
        checkNoDuplicates(a),
        checkNoDuplicates(b)
      )

  private def checkNoDuplicates[A](a: Iterable[A]): Set[A] =
    val seq: Seq[A] = a.toSeq
    if (seq.size != seq.distinct.size)
      throw new IllegalArgumentException(s"duplicates found in collection: $seq")
    seq.toSet

  extension(letters: Seq[Char])
    inline def be(text: String): Unit =
      assertEquals(letters.mkString, text)



