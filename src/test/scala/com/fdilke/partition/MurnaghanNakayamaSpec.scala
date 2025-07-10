package com.fdilke.partition

import com.fdilke.partition.MurnaghanNakayama.{character, height, reductions, sign}
import com.fdilke.utility.RichFunSuite.*
import munit.FunSuite

class MurnaghanNakayamaSpec extends FunSuite:
  test("enumerate the reductions of a given size on 5,2,1"):
    reductions(Seq(5,2,1), 0) is Seq(
      Seq(0, 0, 0)
    )
    reductions(Seq(5,2,1), 1) is Seq(
      Seq(1, 0, 0),
      Seq(0, 1, 0),
      Seq(0, 0, 1)
    )
    reductions(Seq(5,2,1), 2) is Seq(
      Seq(2, 0, 0)
    )
    reductions(Seq(5,2,1), 3) is Seq(
      Seq(3, 0, 0),
      Seq(0, 2, 1)
    )
    reductions(Seq(5,2,1), 4) is Seq.empty

  test("enumerate the reductions of a given size on 5,4,4,3,2"):
    reductions(Seq(5,4,4,3,2), 0) is Seq(
      Seq(0, 0, 0, 0, 0)
    )
    reductions(Seq(5,4,4,3,2), 1) is Seq(
      Seq(1, 0, 0, 0, 0),
      Seq(0, 0, 1, 0, 0),
      Seq(0, 0, 0, 1, 0),
      Seq(0, 0, 0, 0, 1)
    )
    reductions(Seq(5,4,4,3,2), 2) is Seq(
      Seq(0, 1, 1, 0, 0),
      Seq(0, 0, 0, 0, 2)
    )
    reductions(Seq(5,4,4,3,2), 3) is Seq(
      Seq(0, 0, 2, 1, 0),
      Seq(0, 0, 0, 2, 1)
    )
    reductions(Seq(5,4,4,3,2), 4) is Seq(
      Seq(2, 1, 1, 0, 0),
      Seq(0, 1, 2, 1, 0),
      Seq(0, 0, 0, 2, 2)
    )

  test("calculate the height of a border strip (aka reduction)"):
    height(Seq(0, 0, 0)) is -1
    height(Seq(0, 0, 0, 2, 2)) is 1
    height(Seq(3, 0, 0)) is 0

  test("calculate the sign of a height"):
    sign(0) is 1
    sign(7) is -1
    sign(-43) is -1
    sign(-40) is 1
    sign(82) is 1

  test("calculate entries in the character table"):
    // S_0
    character(Seq(), Seq()) is 1
    // S_1
    character(Seq(1), Seq(1)) is 1
    // S_2
    val s2_flip = Seq(1, 1)
    val s2_1 = Seq(2)
    val s2_unit = Seq(1, 1)
    val s2_sign = Seq(2)
    character(s2_flip, s2_unit) is 1
    character(s2_flip, s2_sign) is -1
    character(s2_1, s2_unit) is 1
    character(s2_1, s2_sign) is 1
    // S_3
    val s3_unit = Seq(3)
    val s3_perm = Seq(2, 1)
    val s3_sign = Seq(1, 1, 1)
    val s3_1 = Seq(1, 1, 1)
    val s3_a = Seq(2, 1)
    val s3_r = Seq(3)
    character(s3_unit, s3_1) is 1
    character(s3_unit, s3_a) is 1
    character(s3_unit, s3_r) is 1
    character(s3_perm, s3_1) is 2
    character(s3_perm, s3_a) is 0
    character(s3_perm, s3_r) is -1
    character(s3_sign, s3_1) is 1
    character(s3_sign, s3_a) is -1
    character(s3_sign, s3_r) is 1

    character(Seq(5,2,1), Seq(3,3,1,1)) is -2
