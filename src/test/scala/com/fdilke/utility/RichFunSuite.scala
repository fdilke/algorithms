package com.fdilke.utility

import munit.{ FunSuite, Assertions }
import Assertions.{ assertEquals, assertNotEquals }
// import com.fdilke.bewl2.sets.SetsWithSlowActions
// import com.fdilke.utility.Shortcuts._

object RichFunSuite:
  extension[A] (a: A)
    inline def is(b: A): Unit =
      assertEquals(a, b)
    inline def isnt(b: A): Unit =
      assertNotEquals(a, b)

  extension(letters: Seq[Char])
    inline def be(text: String): Unit =
      assertEquals(letters.mkString, text)

  // import SetsWithSlowActions._

  // implicit class arrowComparisons[A: Dot, B: Dot](arrow: A => B):
  //   inline def isArrow(arrow2: A => B): Unit =
  //     assert(arrow =!= arrow2)
  //   inline def isNotArrow(arrow2: A => B): Unit =
  //     assert(!(arrow =!= arrow2))

  def checkSameElementsAs[X](thing: Seq[X], other: Seq[X]): Unit =
    thing.size is other.size
    thing.toSet is other.toSet
  // extension(text: String)
  //   inline def is(letters: Seq[Char]): Unit =
  //     assertEquals(letters, text.toSeq)



