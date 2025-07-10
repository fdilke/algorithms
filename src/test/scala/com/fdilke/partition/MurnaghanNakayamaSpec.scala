package com.fdilke.partition

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite

class MurnaghanNakayamaSpec extends FunSuite:
  test("enumerate the reductions of a given size on 5,2,1"):
    MurnaghanNakayama.reductions(Seq(5,2,1), 0) is Seq(
      Seq(0, 0, 0)
    )
    MurnaghanNakayama.reductions(Seq(5,2,1), 1) is Seq(
      Seq(1, 0, 0),
      Seq(0, 1, 0),
      Seq(0, 0, 1)
    )
    MurnaghanNakayama.reductions(Seq(5,2,1), 2) is Seq(
      Seq(2, 0, 0)
    )
    MurnaghanNakayama.reductions(Seq(5,2,1), 3) is Seq(
      Seq(3, 0, 0),
      Seq(0, 2, 1)
    )
    MurnaghanNakayama.reductions(Seq(5,2,1), 4) is Seq.empty

  test("enumerate the reductions of a given size on 5,4,4,3,2"):
    MurnaghanNakayama.reductions(Seq(5,4,4,3,2), 0) is Seq(
      Seq(0, 0, 0, 0, 0)
    )
    MurnaghanNakayama.reductions(Seq(5,4,4,3,2), 1) is Seq(
      Seq(1, 0, 0, 0, 0),
      Seq(0, 0, 1, 0, 0),
      Seq(0, 0, 0, 1, 0),
      Seq(0, 0, 0, 0, 1)
    )
    MurnaghanNakayama.reductions(Seq(5,4,4,3,2), 2) is Seq(
      Seq(0, 1, 1, 0, 0),
      Seq(0, 0, 0, 0, 2)
    )
    MurnaghanNakayama.reductions(Seq(5,4,4,3,2), 3) is Seq(
      Seq(0, 0, 2, 1, 0),
      Seq(0, 0, 0, 2, 1)
    )
    MurnaghanNakayama.reductions(Seq(5,4,4,3,2), 4) is Seq(
      Seq(2, 1, 1, 0, 0),
      Seq(0, 1, 2, 1, 0),
      Seq(0, 0, 0, 2, 2)
    )

  test("calculate the height of a border strip (aka reduction)"):
    MurnaghanNakayama.height(Seq(0, 0, 0)) is -1
    MurnaghanNakayama.height(Seq(0, 0, 0, 2, 2)) is 1
    MurnaghanNakayama.height(Seq(3, 0, 0)) is 0
    