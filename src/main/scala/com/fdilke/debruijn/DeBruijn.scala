package com.fdilke.debruijn

/** Calculate a De Bruijn sequence ( https://en.wikipedia.org/wiki/De_Bruijn_sequence )
  * Return a cyclic sequence of length n^k in which every n-sequence from the k-alphabet occurs.
  */
object DeBruijn:
  def apply(n: Int, k: Int): Seq[Int] =
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

