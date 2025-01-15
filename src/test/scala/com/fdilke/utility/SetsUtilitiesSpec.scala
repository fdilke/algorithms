package com.fdilke.utility

import com.fdilke.utility.RichFunSuite.*
import com.fdilke.utility.SetsUtilities.{allMaps, allNaryOps, bulkJoin, intSqrt, sequencesOfLength, squareUp, subsetsOf}
import munit.FunSuite

import java.util.concurrent.atomic.AtomicBoolean

class SetsUtilitiesSpec extends FunSuite:
  test("enumerates all maps between two sets"):
    allMaps(Set(1, 2), Set("a", "b", "c")).map { f =>
        Seq(f(1), f(2))
    }.toSeq is
    Seq(
        Seq("a", "a"),
        Seq("b", "a"),
        Seq("c", "a"),
        Seq("a", "b"),
        Seq("b", "b"),
        Seq("c", "b"),
        Seq("a", "c"),
        Seq("b", "c"),
        Seq("c", "c")
    )

  test("gives sensible results even when the source is empty"):
    allMaps(Set[String](), Set(0)).size is 1

  test("gives sensible results even when the target is empty"):
    allMaps(Set(0), Set()).isEmpty is true

  test("gives sensible results even when both source and target are empty"):
    allMaps(Set(), Set()).size is 1

  test("enumerates n-ary operations: degenerate case of binaries on 1"):
    (allNaryOps(arity = 2, order = 1) map { f =>
      f(0, 0)
    }).toSeq is Seq(0)

  test("enumerates n-ary operations: degenerate case of unaries on 2"):
    (allNaryOps(arity = 1, order = 2) map { f =>
      f(0) -> f(1)
    }).toSeq is
      Seq(
        0 -> 0,
        1 -> 0,
        0 -> 1,
        1 -> 1
      )

  test("enumerates n-ary operations: even more degenerate case of nullaries on 0"):
    allNaryOps(arity = 0, order = 0).toSeq is Seq.empty

  test("enumerates n-ary operations: case of binaries on 2"):
    (allNaryOps(arity = 2, order = 2) map { f =>
      s"${f(0,0)}${f(0,1)}${f(1,0)}${f(1,1)}"
    }).toSet is
      Set(
        "0000", "0001", "0010", "0011", "0100", "0101", "0110", "0111",
        "1000", "1001", "1010", "1011", "1100", "1101", "1110", "1111"
      )
  
  test("enumerate sequences of a given length on given symbols"):
    sequencesOfLength("", 0).toSeq is Seq[Seq[Char]]("")
    sequencesOfLength("a", 0).toSeq is Seq[Seq[Char]]("")
    sequencesOfLength("ab", 1).toSeq is Seq[Seq[Char]]("a", "b")
    sequencesOfLength("ab", 2).toSeq is Seq[Seq[Char]]("aa", "ba", "ab", "bb")
    sequencesOfLength("abc", 2).toSeq is Seq[Seq[Char]](
      "aa", "ba", "ca", 
      "ab", "bb", "cb", 
      "ac", "bc", "cc"
    )
    sequencesOfLength("ab", 3).toSeq is Seq[Seq[Char]](
      "aaa", "baa", 
      "aba", "bba",
      "aab", "bab",
      "abb", "bbb"
    )

  test("enumerate subsets of a set"):
    subsetsOf(Set[Void]()).toSeq is Seq(Set.empty[Void])
    subsetsOf(Set(0)).toSeq is Seq[Set[Int]](Set.empty, Set(0))

    val subsets2: Seq[Set[Int]] = subsetsOf(Set(0, 1)).toSeq
    subsets2.size is 4
    subsets2.toSet is Set[Set[Int]](
      Set.empty, Set(0), Set(1), Set(0, 1)
    )

    val subsets3: Seq[Set[Int]] = subsetsOf(Set(0, 1, 2)).toSeq
    subsets3.size is 8
    subsets3.toSet is Set[Set[Int]](
      Set.empty,
      Set(0), Set(1), Set(2),
      Set(0, 1), Set(0, 2), Set(1, 2),
      Set(0, 1, 2)
    )

  test("bulk join operation, trivial version"):
    bulkJoin[Boolean, Int, String](
      inputs = Seq.empty[Boolean],
      candidates = _ => Iterable(0),
      assignment = (_, _) => "x",
      assignmentZero = "",
      join = { _ + _ }
    ).toSeq is Seq("")

  test("bulk join operation, less trivial version"):
    bulkJoin[Boolean, Int, String](
      inputs = Seq[Boolean](true),
      candidates = _ => Iterable(0),
      assignment = (x, y) => s"<$x:$y>",
      assignmentZero = "",
      join = { _ + _ }
    ).toSeq is Seq("<true:0>")

  test("bulk join operation, more typical version"):
    bulkJoin[Boolean, Int, String](
      inputs = Seq[Boolean](true, false),
      candidates = Map[Boolean, Iterable[Int]](true -> Iterable(1, 2), false -> Iterable(3, 4, 5)),
      assignment = (x, y) => s"<$x:$y>",
      assignmentZero = "",
      join = { _ + _ }
    ).toSeq is Seq(
      "<true:1><false:3>",  "<true:2><false:3>",
      "<true:1><false:4>",  "<true:2><false:4>",
      "<true:1><false:5>",  "<true:2><false:5>"
    )
  
  test("integer square root calculation"):
    intSqrt(0) is 0
    intSqrt(1) is 1
    intercept[IllegalArgumentException]:
      intSqrt(2)
    .getMessage is "2 is not a square"
    intSqrt(4) is 2
    intercept[IllegalArgumentException]:
      intSqrt(20)
    .getMessage is "20 is not a square"
    intSqrt(2025) is 45

  test("can square up an array"):
    intercept[IllegalArgumentException]:
      squareUp(
        true, false, true
      )
    .getMessage is "3 is not a square"
    squareUp(
      1, 7,
      8, 2
    ) is Seq(
      Seq(1, 7),
      Seq(8, 2)
    )
    
