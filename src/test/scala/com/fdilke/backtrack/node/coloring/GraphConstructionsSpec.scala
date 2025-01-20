package com.fdilke.backtrack.node.coloring

import com.fdilke.utility.RichFunSuite._
import munit.FunSuite

import GraphConstructions._

class GraphConstructionsSpec extends FunSuite:
  
  test("torus(0, 0) is empty"):
    torus(0, 0).isEmpty is true
    
  test("torus(0, 3) is empty"):
    torus(0, 3).isEmpty is true
    
  test("torus(3, 0) is empty"):
    torus(3, 0).isEmpty is true
    
  test("torus(1, 1) has 1 vertex"):
    torus(1, 1) is Seq(0 -> 0)



