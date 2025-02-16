package com.fdilke.algebra.permutation

object DihedralGroup:
  def apply(two_n: Int): Group[DihedralSymmetry] =
    if (two_n <= 0)
      throw new IllegalArgumentException("order should be +ve")
    else if (two_n % 2 != 0)
      throw new IllegalArgumentException("order should be even")
    new DihedralGroup(two_n / 2)

// The dihedral group D_2n = automorphisms of an n-gon, parameterised here by n
class DihedralGroup(n: Int) extends Group[DihedralSymmetry]:
  override val unit: DihedralSymmetry =
    DihedralSymmetry.unit(n)

  override val elements: Set[DihedralSymmetry] =
    for {
      sign <- Set(true, false)
      shift <- 0 until n
    } yield
      DihedralSymmetry(n, sign, shift)

  override def multiply(
    element1: DihedralSymmetry,
    element2: DihedralSymmetry
  ): DihedralSymmetry =
    element1 * element2

  override def invert(
    element: DihedralSymmetry
  ): DihedralSymmetry =
    element.invert


