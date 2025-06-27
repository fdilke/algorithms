package com.fdilke.utility

// see also jdk.internal.org.jline.utils.Levenshtein

object Levenshtein:
  def distance(string1: String, string2: String): Int =
    val s1: Array[Char] = string1.toCharArray
    val s2: Array[Char] = string2.toCharArray
    var prev = new Array[Int](s2.length + 1)
    for
      j <- 0 to s2.length
    do
      prev(j) = j
    for
      i <- 0 until s1.length
    do
      val curr = new Array[Int](s2.length + 1)
      curr(0) = i + 1
      for (j <- s2.indices) {
        val d1 = prev(j + 1) + 1
        val d2 = curr(j) + 1
        var d3 = prev(j)
        if s1(i) != s2(j) then
          d3 += 1
        curr(j + 1) = Math.min(Math.min(d1, d2), d3)
      }
      prev = curr
    prev(s2.length)

