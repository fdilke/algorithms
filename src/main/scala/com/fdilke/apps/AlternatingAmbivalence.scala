package com.fdilke.apps

import com.fdilke.algebra.permutation.Permutation

object AlternatingAmbivalence extends App:
  // 0, 1, 2, 5, 6, 10
  (0 until 11).foreach: n =>
    if Permutation.alternatingGroup(n).isAmbivalent then
      print(n.toString + " ")
    else
      print("(not " + n.toString + ") ")

object AlternatingStrongAmbivalence extends App:
  // 0 1 2 but not any of the others
  (0 until 11).foreach: n =>
    if Permutation.alternatingGroup(n).isStronglyAmbivalent then
      print(n.toString + " ")
    else
      print("(not " + n.toString + ") ")

