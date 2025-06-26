package com.fdilke.apps

import com.fdilke.utility.Levenshtein

object LevenshteinTriangle extends App:
  val words: Set[String] = Set(
    "a", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog",
    "now", "is", "time", "for", "all", "good", "men", "to", "aid", "party",
    "an", "and"
  )
  val triples: Set[Seq[Int]] =
    for
      word1 <- words
      word2 <- words if word2 != word1
      word3 <- words if word3 != word1 && word3 != word2
    yield
      Seq(
        Levenshtein.distance(word1, word2),
        Levenshtein.distance(word2, word3),
        Levenshtein.distance(word1, word3)
      ).sorted
  val showTriples =
    triples.toSeq.sorted.map: triple =>
      triple.mkString("")
  println(s"showTriples:\n $showTriples")