package com.fdilke.algebra.field

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite

class FiniteFieldSpec extends FunSuite:

  test("can recognize nontrivial prime powers"):
    val NontrivialPrimePower(q, r) = 7 : @unchecked
    q is 7
    r is 1

    val NontrivialPrimePower(p, n)  = 81 : @unchecked
    p is 3
    n is 4

    intercept[MatchError]:
      val NontrivialPrimePower(p, n) = 1 : @unchecked

    intercept[MatchError]:
      val NontrivialPrimePower(p, n) = 6 : @unchecked

  test("rejects request for a non-prime-power-sized field"):
    intercept[IllegalArgumentException]:
      FiniteField.GF(6)

  test("can load the integers mod 5"):
    val field5: Field[Int] = FiniteField.GF(5)

    import field5.{ I, O }
    val I2: Int = field5.add(I, I)
    val I3: Int = field5.add(I, I2)
    val I4: Int = field5.add(I2, I2)
    field5.elements.size is 5
    field5.elements is Seq(
      O, I, I2, I3, I4
    )
    field5.multiply(I2, I3) is I

  test("can load GF(4) in detail"):
    val field4: Field[Int] =
      FiniteField.GF(4)

    import field4.{ O, I }
    field4.elements.size is 4
    val Seq(o, i, a, b) = field4.elements
    o is O
    i is I
    b is field4.add(a, I)
    a is field4.add(b, I)
    field4.multiply(a, a) is b
    field4.multiply(b, b) is a

  test("can load GF(4)"):
    testField(4)

  test("can load GF(27)"):
    testField(27)

  test("can load GF(81)"):
    testField(81)

// pass, but are too slow
//  test("can load GF(243)"):
//    testField(243)
//
//  test("can load GF(343)"):
//    testField(343)

  def testField(pn: Int): Unit =
    val field = FiniteField.GF(pn)
    import field.{ I, O }
    field.elements.size is pn
    field.elements.foreach: a =>
      field.add(a, O) is a
      field.add(a, field.minus(a)) is O
      field.subtract(a, a) is O
      field.multiply(a, O) is O
      field.multiply(a, I) is a
      if (a != O)
        field.multiply(a, field.invert(a)) is I
      field.elements.foreach: b =>
        field.add(a, b) is field.add(b, a)
        field.multiply(a, b) is field.multiply(b, a)
        field.add(field.subtract(a, b), b) is a
        if (b != O)
          field.multiply(field.divide(a, b), b) is a
        field.elements.foreach: c =>
          field.add(field.add(a, b), c) is field.add(a, field.add(b, c))
          field.multiply(field.multiply(a, b), c) is field.multiply(a, field.multiply(b, c))
          field.multiply(field.add(a, b), c) is field.add(field.multiply(a, c), field.multiply(b, c))

  test("can enumerate matrices"):
    val field2: Field[Int] = FiniteField.GF(2)
    field2.squareMatrices(0).toSeq.map(_.toString) is:
      Seq("")
    field2.squareMatrices(1).toSeq.map(_.toString) is:
      Seq("0", "1")
    field2.squareMatrices(2).toSeq.map(_.toString) is:
      Seq(
        "00/00", "10/00", "01/00", "11/00",
        "00/10", "10/10", "01/10", "11/10",
        "00/01", "10/01", "01/01", "11/01",
        "00/11", "10/11", "01/11", "11/11"
      )

  test("can calculate matrix determinants over GF(4)"):
    val field4: Field[Int] = FiniteField.GF(4)
    val Seq(o, i, a, b) = field4.elements
    field4.determinant(
    ) is i
    field4.determinant(
      a
    ) is a
    field4.determinant(
      o, i,
      a, b
    ) is a
    field4.determinant(
      i, a,
      b, 0
    ) is i
    field4.determinant(
      o, i, a,
      i, 0, b,
      i, i, i
    ) is 0

  test("can calculate matrix determinants over GF(9)"):
    val field9: Field[Int] = FiniteField.GF(9)
    // field9.showTables()
    field9.determinant(
      7
    ) is 7
    field9.determinant(
      6, 5,
      7, 3
    ) is 4
    field9.determinant(
      1, 5,
      2, 3
    ) is 8
    field9.determinant(
      1, 6,
      2, 7
    ) is 4
    field9.determinant(
      0, 8, 4,
      1, 6, 5,
      2, 7, 3
    ) is 0
