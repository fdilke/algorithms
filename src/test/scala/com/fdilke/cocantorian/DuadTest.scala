package com.fdilke.cocantorian

import munit.FunSuite

import scala.language.postfixOps

class DuadTest extends FunSuite {
  test("token initial test") {
    assert { 1 == 1 }
  }
  
/*  
  describe("Duad helper functions") {
    it("can tell if a number is a power of 2") {
      Seq(
        0, 1, 2, 3, 4, 5, 8, 17, 32
      ).map(isPowerOf2) shouldBe Seq(
        false, true, true, false, true, false, true, false, true
      )
    }
    it("can get a Duadic sequence into canonical form") {
      canonical("x") shouldBe (Seq("x"), 1)
      canonical("A", "A") shouldBe (Seq("A"), 1)
      canonical(true, true) shouldBe (Seq(true), 1)
      canonical(1.0, 2.0) shouldBe (Seq(1.0, 2.0), 2)
      canonical(1.0, 2.0, 1.0, 2.0) shouldBe (Seq(1.0, 2.0), 2)
      canonical(1.0, 2.0, 3.0, 4.0) shouldBe (Seq(1.0, 2.0, 3.0, 4.0), 4)
      canonical(4.0, 4.0, 4.0, 4.0) shouldBe (Seq(4.0), 1)
      canonical(10, 15, 10, 15, 10, 15, 10, 15) shouldBe (Seq(10, 15), 2)
      canonical(6, 5, 0, 2, 6, 5, 0, 2) shouldBe (Seq(6, 5, 0, 2), 4)
      canonical[Boolean => Int](
        if (_) 4 else 4,
        b => (if (b) 8 else 9) / 2
      )._2 shouldBe 1
    }
  }
  describe("Duads") {
    it("can't be instantiated using 'new'") {
      """new Duad[String](Seq("X", "Y"), 2)""" shouldNot compile
    }
    it("can be instantiated using a companion factory method") {
      Duad(2).getClass shouldBe classOf[Duad[_]]
    }
    it("can be instantiated only with sequences of length a power of 2") {
      Duad(1)
      Duad(4, 4, 4, 4)
      Duad(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16)
      intercept[IllegalArgumentException] {
        Duad(7, 7, 7, 7, 7, 7, 7)
      }
    }
    it("have sane equality semantics") {
      Duad("A", "B") shouldBe Duad("A", "B")
      Duad("A", "B") shouldNot be(Duad("A"))
      Duad("B") shouldNot be(Duad("A"))
      Duad("A", "B") shouldNot be(Duad("B", "A"))
    }
    it("have a presentable toString method") {
      Duad("A", "B").toString shouldBe "Duad(A,B)"
    }
    it("are instantiated using canonical form") {
      Duad("A", "A") shouldBe Duad("A")
      Duad("A", "A", "A", "A") shouldBe Duad("A")
      Duad("A", "B", "C", "D", "A", "B", "C", "D") shouldBe Duad("A", "B", "C", "D")
      Duad("A", "B", "A", "B", "A", "B", "A", "B") shouldBe Duad("A", "B")
    }
    it("support map which coalesces the result into canonical form") {
      Duad("foo", "barbaz").map(_.length) shouldBe Duad(3, 6)
      Duad(1, 2, 3, 4).map(_ % 2) shouldBe Duad(1, 0)
      Duad[Boolean => Int](
        if (_) 4 else 4,
        b => (if (b) 8 else 9) / 2
      ) shouldBe Duad[Boolean => Int](
        if (_) 4 else 4,
        b => (if (b) 8 else 9) / 2
      )
    }
    it("have a length") {
      Duad(1).length shouldBe 1
      Duad(1, 7).length shouldBe 2
      Duad(1, 7, 1, 7).length shouldBe 2
      Duad(1, 7, 7, 1).length shouldBe 4
    }
    it("can be treated as (doubly infinite) sequences") {
      val Duad: Duad[String] =
        Duad("along", "came", "a", "spider")

      Duad(0) shouldBe "along"
      Duad(4) shouldBe "along"
      Duad(2) shouldBe "a"
      Duad(-3) shouldBe "came"
      Duad(287) shouldBe "spider"
      Duad(-666) shouldBe "a"
    }
    it("have a η and μ obeying the monad identity laws") {
      val Duad: Duad[Int] =
        Duad(1, 2, 3, 4)

      μ[Int](
        η[Duad[Int]](Duad)
      ) shouldBe Duad
      μ[Int](
        Duad.map(η[Int])
      ) shouldBe Duad
    }
    it("also μ obeys the monad associativity law") {
      val Duad: Duad[Duad[Duad[Int]]] =
        Duad(
          Duad(
            Duad(7, 2, 3, 4),
            Duad(1, 8)
          ),
          Duad(
            Duad(5, 3, 2, 1)
          )
        )

      μ[Int](
        Duad.map(μ[Int])
      ) shouldBe
        μ[Int](
          μ[Duad[Int]](Duad)
        )
    }
    it("can be used in for-comprehensions to exploit the monad operations") {
      (for {
        x <- Duad("one", "at", "too", "on")
      } yield {
        x.length
      }) shouldBe
        Duad(3, 2)

      (for {
        x <- Duad("post", "captain")
        y <- Duad("by", "Patrick", "O", "Brian")
      } yield {
        x + " " + y
      }) shouldBe
        Duad(
          "post by",
          "captain Patrick",
          "post O",
          "captain Brian"
        )
    }
    it("have a Jonsson-Tarski structure that obeys the axioms") {
      left(Duad(1)) shouldBe Duad(1)
      right(Duad(1)) shouldBe Duad(1)

      val Duad: Duad[Int] =
        Duad(1, 2, 3, 4)
      val Duad2 = Duad.map {
        _ + 10
      }

      left(Duad) shouldBe Duad(1, 3)
      right(Duad) shouldBe Duad(2, 4)

      join(Duad, Duad2) shouldBe Duad(
        1, 11, 2, 12, 3, 13, 4, 14
      )

      join(left(Duad), right(Duad)) shouldBe Duad

      left(join(Duad, Duad2)) shouldBe Duad
      right(join(Duad2, Duad)) shouldBe Duad
    }
    it("inherit Hausdorffness from their type parameter") {
      equalH(
        Duad[Boolean => Int](
          _ => 4,
          if (_) 20 else 18
        ),
        Duad[Boolean => Int](
          if (_) 1 else 3,
          _ => 2
        )
      ) shouldBe false
      equalH(
        Duad[Boolean => Int](_ => 4),
        Duad[Boolean => Int](
          if (_) 4 else 4,
          b => (if (b) 8 else 9) / 2
        )
      ) shouldBe true
    }
    it("have Catcher nature") {
      val DuadIntCatcher =
        Catcher[Duad[Int], Boolean, Int]

      DuadIntCatcher.either(
        Duad(2)
      ) shouldBe Left(2)

      DuadIntCatcher.either(
        Duad(2, 3)
      ) match {
        case Left(_) => fail("Unexpected singleton Duad")
        case Right(fn) =>
          DuadIntCatcher.either(fn(false)) shouldBe Left(2)
          DuadIntCatcher.either(fn(true)) shouldBe Left(3)
      }

      DuadIntCatcher.construct(Left(2)) shouldBe Duad(2)
      DuadIntCatcher.construct(Right(boolean => if (boolean) Duad(3) else Duad(4, 5))) shouldBe Duad(
        4,
        3,
        5,
        3
      )
    }
    it("act on Cantorians and other pitchers") {
      val Duad: Duad[Int] =
        Duad(2, 4, 7, 1, 9, 0, 3, 5)

      Duad(Cantorian.cycle(true)) shouldBe 5
      Duad(Cantorian.cycle(false)) shouldBe 2
      Duad(Cantorian.cycle(true, false)) shouldBe 0
      Duad(Cantorian.cycle(false, true)) shouldBe 7

      Duad(Pitcher.constantly[VanillaPitcher[Boolean], Boolean](true)) shouldBe 5
      Duad(Pitcher.constantly[VanillaPitcher[Boolean], Boolean](false)) shouldBe 2
    }
  }
*/  
}
