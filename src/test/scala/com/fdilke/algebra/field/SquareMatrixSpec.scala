package com.fdilke.algebra.field

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite

class SquareMatrixSpec extends FunSuite:
  test("can create and distinguish matrices"):
    intercept[IllegalArgumentException]:
      SquareMatrix[String]("not a", "square")
    val matrix0110 = SquareMatrix[Int](0, 1, 1, 0)
    val matrix1001 = SquareMatrix[Int](1, 0, 0, 1)
    val matrix0110_2 = SquareMatrix[Int](0, 1, 1, 0)
    matrix0110 is matrix0110
    matrix0110 is matrix0110_2
    matrix0110 isnt matrix1001

  test("can read entries in the right order, following Herstein"):
    val matrix0123 = SquareMatrix[Int](0, 1, 2, 3)
    matrix0123(0)(0) is 0
    matrix0123(0)(1) is 1
    matrix0123(1)(0) is 2
    matrix0123(1)(1) is 3

  test("can initialize matrices as square arrays and display them in string form"):
    SquareMatrix[Int](
      0, 1,
      2, 3
    ).toString is
      "01/23"

  test("can measure the order (size) of a matrix"):
    SquareMatrix[Int](
      0, 1,
      2, 3
    ).order is 2
