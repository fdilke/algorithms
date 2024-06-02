package com.fdilke.debruijn

trait DeBruijn:
  def apply(n: Int, k: Int): Seq[Int]

  final def string(n : Int, k: Int): String =
    apply(n, k).map { i => ('0' + i).toChar }.mkString

// Calculate a De Bruijn sequence ( https://en.wikipedia.org/wiki/De_Bruijn_sequence )
// Return a cyclic sequence of length n^k in which every n-sequence from the k-alphabet occurs.
