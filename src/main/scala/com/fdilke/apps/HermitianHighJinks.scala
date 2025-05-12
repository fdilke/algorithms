package com.fdilke.apps

import org.apache.commons.math3.linear.{EigenDecomposition, MatrixUtils, RealMatrix}

object HermitianHighJinks extends App:
  val realMatrix: RealMatrix =
    MatrixUtils.createRealMatrix(
      Array[Array[Double]](
        Array[Double](0.0, 1.0),
        Array[Double](1.0, 0.0)
      )
    )
  val eigen =
    new EigenDecomposition(realMatrix)
  println(s"determinant = ${eigen.getDeterminant}")
  println(s"eigenvalues = ${eigen.getRealEigenvalues.mkString("Array(", ", ", ")")}")
  for { i <- 0 until realMatrix.getRowDimension }
    println(s"eigenvector: ${eigen.getEigenvector(i)}")
