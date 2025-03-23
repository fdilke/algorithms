package com.fdilke.blocks

import com.fdilke.utility.SetsUtilities.nCr

object DivisibilityConditions:
  def apply(
    lambda: Int,
    r: Int,
    q: Int,
    n: Int
  ): Boolean =
    (0 until r) forall: i =>
      (lambda * nCr(n - i, r - i)) % nCr(q - i, r - i) == 0  
