package com.fdilke.algebra.permutation

import com.fdilke.utility.RichFunSuite._
import scala.language.postfixOps
import munit.FunSuite

class GroupSpec extends FunSuite:
  test("can calculate the subgroup generated by a subset"):
    val group: Group[Permutation] =
      Permutation.symmetricGroup(3)

    val transposition = Permutation(0, 2, 1)

    group.generateSubgroup() is group.trivialSubgroup

    group.generateSubgroup(
      transposition
    ).elements is Set(
      group.unit,
      transposition
    )

    val rotation = Permutation(1, 2, 0)

    group.generateSubgroup(
      transposition,
      rotation
    ).elements is
      group.elements

  test("can tell if a group is cyclic"):
    CyclicGroup(9).isCyclic is true
    DihedralGroup(4).isCyclic is false
    Permutation.symmetricGroup(3).isCyclic is false

  test("can tell if a group is abelian"):
    CyclicGroup(9).isAbelian is true
    DihedralGroup(4).isAbelian is true
    Permutation.symmetricGroup(3).isAbelian is false

  test("can compute the centre for cyclic groups"):
    val group = CyclicGroup(9)
    group.centre is group.wholeGroup

  test("can compute the centre for symmetric groups"):
    val group = Permutation.symmetricGroup(4)
    group.centre is group.trivialSubgroup

  test("can compute the centre for dihedral groups"):
    val group = DihedralGroup(4)
    group.centre is group.wholeGroup

  test("can compute the centre for bigger dihedral groups"):
    DihedralGroup(6).centre.order is 1
    DihedralGroup(12).centre.order is 2

  test("can compute the order of elements - for cyclic groups"):
    CyclicGroup(9).elementOrders is Map(
      1 -> 1,
      9 -> 6,
      3 -> 2
    )

  test("can compute the order of elements - for dihedral groups"):
    DihedralGroup(6).elementOrders is Map(
      1 -> 1,
      2 -> 3,
      3 -> 2
    )

  test("determine a complement for a subgroup, if there is one"):
    val group9: Group[Int] =
      CyclicGroup(9)
    group9.trivialSubgroup.hasComplement is Some(group9.wholeGroup)
    group9.wholeGroup.hasComplement is Some(group9.trivialSubgroup)
    val ord3: Int =
      group9.elementOfOrder(3)
    group9.generateSubgroup(ord3).hasComplement is None

  test("determine a complement for a subgroup, if there is one (2)"):
    val group6: Group[Permutation] =
      Permutation.symmetricGroup(3)
    val ord3: Permutation =
      group6.elementOfOrder(3)
    val ord2: Permutation =
      group6.elementOfOrder(2)
    group6.generateSubgroup(ord3).hasComplement.isDefined is true
    group6.generateSubgroup(ord2).hasComplement is Some(group6.generateSubgroup(ord3))

  test("determine a complement for a subgroup, if there is one (3)"):
    val group: Group[Permutation] =
      Permutation.symmetricGroup(4)
    for
      subgroup <- group.subgroups if !Set(2,4).contains(subgroup.order)
    do
      subgroup.hasComplement.isDefined is true

  test("determine a complement for a subgroup, if there is one (4)"):
    val group: Group[Permutation] =
      Permutation.alternatingGroup(4)
    for
      subgroup <- group.subgroups
    do
      subgroup.hasComplement.isEmpty is (subgroup.order == 2)

  test("enumerating subgroups of the trivial group"):
    val trivialGroup = Permutation.symmetricGroup(1)
    trivialGroup.subgroups is Set(
      trivialGroup.trivialSubgroup
    )

  test("enumerating subgroups of the 2-element group"):
    val twoGroup = Permutation.symmetricGroup(2)
    twoGroup.subgroups is Set(
      twoGroup.trivialSubgroup,
      twoGroup.wholeGroup
    )

  // return the orders of a group's subgroups -
  // with the sign reversed if they're normal!
  // and ordered with the normal subgroups last for each order
  private def subgroupOrdersX[T](group: Group[T]): Seq[Int] =
    group.subgroups.toSeq.map: subgroup =>
      subgroup.order * (
        if (subgroup.isNormal) -1 else +1
      )
    .sortBy: orderX =>
      3 * Math.abs(orderX) - (
        if (orderX > 0) +1 else -1
      )

  test("enumerating subgroups of cyclic groups works - at least orders are correct"):
    subgroupOrdersX(CyclicGroup(6)) is Seq(-1,-2,-3,-6)
    subgroupOrdersX(CyclicGroup(7)) is Seq(-1,-7)
    subgroupOrdersX(CyclicGroup(8)) is Seq(-1,-2,-4,-8)

  test("enumerating subgroups of symmetric groups works"):
    subgroupOrdersX(Permutation.symmetricGroup(3)) is Seq(
      -1, 2, 2, 2, -3, -6
    )

  test("enumerating subgroups of dihedral groups works"):
    subgroupOrdersX(DihedralGroup(12)) is Seq(
      -1, 2, 2, 2, 2, 2, 2, -2, -3, 4, 4, 4, -6, -6, -6, -12
    )

  test("can compute stabilizers in a permutation group"):
    val group: Group[Permutation] =
      Permutation.symmetricGroup(4)
    group.stabilizer() is group.wholeGroup
    group.stabilizer(0).order is 6
    group.stabilizer(0, 1).order is 2
    group.stabilizer(0, 1, 2) is group.trivialSubgroup
    group.stabilizer(0, 1, 2, 3) is group.trivialSubgroup
