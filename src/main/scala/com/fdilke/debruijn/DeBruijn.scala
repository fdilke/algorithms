package com.fdilke.debruijn


// Calculate a De Bruijn sequence ( https://en.wikipedia.org/wiki/De_Bruijn_sequence )
// Return a cyclic sequence of length n^k in which every n-sequence from the k-alphabet occurs.
object DeBruijn:
  
    // original Python algo to generate 'sequence' (from Wikipedia page above)
    // a = [0] * k * n
    // sequence = []
    // def db(t, p):
    //     if t > n:
    //         if n % p == 0:
    //             sequence.extend(a[1 : p + 1])
    //     else:
    //         a[t] = a[t - p]
    //         db(t + 1, p)
    //         for j in range(a[t - p] + 1, k):
    //             a[t] = j
    //             db(t + 1, t)

    // db(1, 1)

  def string(n : Int, k: Int): String =
    apply(n, k).map { i => ('0' + i).toChar }.mkString

  def apply(n: Int, k: Int): Seq[Int] =
    if k == 1 then
      Seq(0)
    else
      val a: Array[Int] = new Array(n * k)
      var sequence: Seq[Int] = Seq.empty
      def db(t: Int, p: Int): Unit =
        if t > n then
          if (n % p == 0) then
            sequence ++= (1 to p) map a
        else
          a(t) = a(t - p)
          db(t + 1, p)
          for { j <- (a(t - p) + 1) until k }
            a(t) = j
            db(t + 1, t)
      db(1, 1)
      sequence
