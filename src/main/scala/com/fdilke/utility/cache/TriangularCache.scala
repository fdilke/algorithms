package com.fdilke.utility.cache

import scala.reflect.ClassTag

// Cache a function f(m, n) of two integers with 0 <= m < n,
// by storing the results in an array which can be dynamically resized.

class TriangularCache[RESULT: ClassTag](
  f: (Int, Int) => RESULT
) extends ((Int, Int) => RESULT):
  private var size = 0
  private var array: Array[Array[RESULT]] =
    Array.fill(0):
      Array.fill(0)(null.asInstanceOf[RESULT])

  override def apply(m: Int, n: Int): RESULT =
    if n >= size then
      array =
        Array.tabulate(n + 1): i =>
          if i < size then
            array(i)
          else
            Array.fill(i)(null.asInstanceOf[RESULT])
      size = n + 1
    Option(array(n)(m)) match
      case None =>
        val result: RESULT = f(m, n)
        array(n)(m) = result
        result
      case Some(result) =>
        result



