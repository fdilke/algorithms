package com.fdilke.debruijn

import scala.collection.mutable
import java.util.Vector

// Calculate a De Bruijn sequence ( https://en.wikipedia.org/wiki/De_Bruijn_sequence )
// Return a cyclic sequence of length n^k in which every n-sequence from the k-alphabet occurs.
object DeBruijn:
  def apply(n: Int, k: Int): Seq[Int] =
    val alphabet = (0 until k).map { i => ('0' + i).toChar }.mkString
    val string = deBruijn(n, k, alphabet)
    string.toCharArray.toSeq.map { c => c - '0' }

  private val seen: mutable.Set[String] = new mutable.HashSet[String]()
  private val edges: Vector[Integer] = new Vector[Integer]()
 
  // Function to find a de Bruijn sequence
  // of order n on k characters
  def deBruijn(n : Int, k: Int, a: String): String =
      // Clearing global variables
      seen.clear()
      edges.clear()

      val startingNode: String = string(n - 1, a.charAt(0))
      dfs(startingNode, k, a)

      var s: String = ""

      // Number of edges
      val l: Int = Math.pow(k, n).toInt
      for { i <- 0 until l }
          s = s + a.charAt(edges.get(i))
      s += startingNode
      s

    // Modified DFS in which no edge
    // is traversed twice
  def dfs(node: String, k: Int, a: String): Unit =
      for { i <- 0 until k }
          val str: String = node + a.charAt(i)
          if (!seen.contains(str)) then
              seen.add(str)
              dfs(str.substring(1), k, a)
              edges.add(i)

  def string(n : Int, charAt: Char): String =
      var str: String = ""
      for { i <- 0 until n } 
          str = str + charAt
      str


  def badApply(n: Int, k: Int): Seq[Int] =
    val a: Array[Int] = Array.ofDim(n * k + 1)
    println("a = " + a.toSeq)
    var seq: Seq[Int] = Seq.empty
    def db(t: Int, p : Int): Unit =
      if (t > n) {
        if (n % p == 0) {
          println("doing thing")
          seq ++= a.toSeq.slice(1, p + 1)
        }
      } else {
            a(t) = a(t - p)
            db(t + 1, p)
            for (j <- a(t - p) + 1 to k) {
                a(t) = j
                db(t + 1, t)    
            }
      }
    db(1, 1)
    println("seq = " + seq)
    // seq
    Seq.empty

    
  // def apply(n: Int, k: Int): Seq[Int] =
  //   apply(n, 0 until k)

  // def apply[T](
  //   n: Int,
  //   alphabet: Seq[T]
  // ): Seq[T] =
  //   val k = alphabet.length
  //   val a: Array[Int] =
  //   Seq.empty

