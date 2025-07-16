package com.fdilke.utility.cache

import scala.reflect.ClassTag

// Cache a function f(n) of one integer with 0 <= n,
// by storing the results in an array which can be dynamically resized.

class LinearCache[RESULT: ClassTag](
  f: Int => RESULT
) extends (Int => RESULT):
  private var size = 1
  private var array: Array[RESULT] =
    Array.fill(size)(null.asInstanceOf[RESULT])

  override def apply(n: Int): RESULT =
    while n >= size do
      array =
        Array.tabulate(size * 2): i =>
          if i < size then
            array(i)
          else
            null.asInstanceOf[RESULT]
      size *= 2
    Option(array(n)) match
      case None =>
        val result: RESULT = f(n)
        array(n) = result
        result
      case Some(result) =>
        result



