package com.fdilke.utility

import RichFunSuite.*
import com.fdilke.utility.Levenshtein.distance
import munit.FunSuite

class LevenshteinSpec extends FunSuite:

  test("distance calculation works"):
    distance("", "") is 0
    distance("a", "") is 1
    distance("a", "b") is 1
    distance("a", "ab") is 1
    distance("a", "bc") is 2
    distance("fox", "fox") is 0
    distance("fox", "fax") is 1
    distance("folx", "fax") is 2
    distance("switch", "with") is 2
    distance("felix the cat", "anita the hamster") is 10
    distance("kitten", "sitting") is 3
    distance("intention", "execution") is 5

