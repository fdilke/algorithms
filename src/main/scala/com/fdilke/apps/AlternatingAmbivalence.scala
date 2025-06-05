package com.fdilke.apps

import com.fdilke.algebra.permutation.Permutation

object AlternatingAmbivalence extends App:
  (0 until 11).foreach: n =>
    if Permutation.alternatingGroup(n).isAmbivalent then
      print(n.toString + " ")
    else
      print("(not " + n.toString + ") ")

