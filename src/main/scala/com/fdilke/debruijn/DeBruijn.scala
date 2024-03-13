package com.fdilke.debruijn

/** Calculate a De Bruijn sequence ( https://en.wikipedia.org/wiki/De_Bruijn_sequence )
  * Return a cyclic sequence of length n^k in which every n-sequence from the k-alphabet occurs.
  */
object DeBruijn:
  def apply(n: Int, k: Int): Seq[Seq[Int]] =
    Seq.empty

