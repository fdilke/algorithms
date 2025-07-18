package com.fdilke.utility.cache

import munit.FunSuite

import java.util.concurrent.atomic.AtomicInteger

class MemoizeSpec extends FunSuite:

  class LocalScope {
    val callCount: AtomicInteger =
      AtomicInteger(0)

    def callCountIs(expected: Int): Unit =
      assertEquals(
        callCount.get(),
        expected
      )

    def vanillaFn(text: String): Int =
      callCount.incrementAndGet()
      text.length

    val memoizedVanilla: String => Int =
      Memoize(vanillaFn)

    def composite[X, Y](
                         setXsetY: (Set[X], Set[Y])
                       ): Set[X | Y] =
      callCount.incrementAndGet()
      val (setX, setY) = setXsetY

      setX map { x =>
        x : X | Y
      } union (
        setY map { y =>
          y : X | Y
        }
      )

    def takeHead[X](
                     list: List[X]
                   ): X =
      callCount.incrementAndGet()
      list.head

    val memoizedHead:
      [X] => List[X] => X
    = Memoize.type1[
      [X] =>> List[X],
      [X] =>> X
    ](
      [X] => (list: List[X]) => takeHead(list)
    )

    val memoizedComposite:
      [X, Y] => ((Set[X], Set[Y])) => Set[X | Y]
    = Memoize.type2[
      [X, Y] =>> (Set[X], Set[Y]),
      [X, Y] =>> Set[X | Y]
    ](
      [X, Y] => (sets: (Set[X], Set[Y])) => composite[X, Y](sets)
    )
  }

  private val fixture: FunFixture[LocalScope] =
    FunFixture[LocalScope](
      setup = { test =>
        new LocalScope
      },
      teardown = { scope => () }
    )

  fixture.test("memoized vanilla function acts as pass through,is cached") { scope =>
    import scope.*

    def confirmVanilla(): Unit =
      assertEquals(
        memoizedVanilla("hello"),
        5
      )

    callCountIs(0)
    confirmVanilla()
    callCountIs(1)
    confirmVanilla()
    callCountIs(1)
  }

  fixture.test("memoized function with 1 type arg acts as pass through") { scope =>
    import scope.*

    val input =
      List[Int](1, 2, 3)
    val output =
      1

    def confirmHead(): Unit =
      assertEquals(
        memoizedHead[Int](
          input
        ),
        output
      )

    callCountIs(0)
    confirmHead()
    callCountIs(1)
    confirmHead()
    callCountIs(1)
  }

  fixture.test("memoized function with 2 type args acts as pass through") { scope =>
    import scope.*

    val input =
      Set(1,2,3) -> Set("hello", "goodbye")
    val output =
      Set[Int | String](
        1, 2, 3, "hello", "goodbye"
      )

    def confirmComposite(): Unit =
      assertEquals(
        memoizedComposite[Int, String](
          input
        ),
        output
      )

    callCountIs(0)
    confirmComposite()
    callCountIs(1)
    confirmComposite()
    callCountIs(1)
  }


