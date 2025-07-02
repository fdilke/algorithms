package com.fdilke.partition

object Partitions:
  // Return all sorted tuples of integers >= min > 0 which sum to total
  def apply(min: Int, total: Int): Seq[Seq[Int]] =
    if total == 0 then
      Seq(Seq())
    else if min == total then
      Seq(Seq(total))
    else if min > total then
      Seq()
    else
      val partitions: Seq[Seq[Seq[Int]]] =
        for
          m <- min to total
        yield apply(m, total - m) map:
            p => m +: p
      partitions.reduce:
        _ ++ _

  // Return all sorted tuples of integers >= 0 which sum to total
  def apply(n: Int): Seq[Seq[Int]] =
    apply(1, n)

