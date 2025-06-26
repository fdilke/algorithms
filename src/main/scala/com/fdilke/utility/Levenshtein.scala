package com.fdilke.utility

object Levenshtein:
  def distance(string1: String, string2: String): Int =
    val s1: Array[Char] = string1.toCharArray
    val s2: Array[Char] = string2.toCharArray
    var prev = new Array[Int](s2.length + 1)
    for
      j <- 0 until s2.length + 1
    do
      prev(j) = j
    for
      i <- 1 until s1.length + 1
    do
      val curr = new Array[Int](s2.length + 1)
      curr(0) = i
      for (j <- 1 until s2.length + 1) {
        val d1 = prev(j) + 1
        val d2 = curr(j - 1) + 1
        var d3 = prev(j - 1)
        if (s1(i - 1) != s2(j - 1)) d3 += 1
        curr(j) = Math.min(Math.min(d1, d2), d3)
      }
      prev = curr
    prev(s2.length)

