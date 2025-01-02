package com.fdilke.utility

import Math.min

object NumberCrunch:
  def confineTo(n: Int, modulus: Int): Int =
    (n % modulus + modulus) % modulus

