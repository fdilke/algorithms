package com.fdilke.apps

import com.fdilke.partition.{MurnaghanNakayama, Partitions}

//type NamedPartition = (name: String, partition: Seq[Int])
//(
//  name = name,
//  partition = p
//)

object SymmetricRepresentations extends App:
  def showCharacterTable(n: Int): Unit =
    val partitions: Seq[Seq[Int]] =
      Partitions.antiLex(n)
    val labelA: Int =
      Char.char2int('a')
    def label(i: Int): String =
      (labelA + i).toChar.toString
    val table: Seq[Seq[Int]] =
      partitions.map: p =>
        partitions.map: q =>
          MurnaghanNakayama.character(p, q)
    val width: Int =
      1 + table.flatten.map { _.toString.length }.max
    val spacing: String =
      " " * (width - 1)
    def lpad(c: Char, n: Int)(s: String): String =
      (c.toString * (n - s.length)) + s
    def show(v: Int): String =
      val initial =
        if v >= 0 then
          " " + v.toString
        else
          v.toString
      if initial.length > width then
        "x" * width
      else
        lpad(' ', width)(initial)
    print(s"$n) ")
    for
      i <- partitions.indices
    do
      print(s"${label(i)}=${partitions(i).mkString("")} ")
    println()
    println(s"\\ $spacing${partitions.indices.map(label).mkString(spacing)}")
    for
      i <- partitions.indices
    do
      val entries: Seq[Int] =
        partitions.indices.map: j =>
          table(i)(j)
      println(s"${label(i)} ${entries.map(show).mkString("")}")

  for
    n <- 2 to 7
  do
    showCharacterTable(n)
    println("----")
