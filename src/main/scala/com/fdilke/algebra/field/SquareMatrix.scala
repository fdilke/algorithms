package com.fdilke.algebra.field

import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.intSqrt

import java.util
import scala.reflect.ClassTag

class SquareMatrix[T](
  protected val matrix: Array[Array[T]]
):
  override def equals(other: Any): Boolean =
    other match
      case otherMatrix: SquareMatrix[T] =>
        otherMatrix.isEqual(this)
      case _ =>
        throw new IllegalArgumentException("bad comparison")

  private def isEqual(other: SquareMatrix[T]): Boolean =
    util.Arrays.deepEquals(
      matrix.asInstanceOf[Array[Any]],
      other.matrix.asInstanceOf[Array[Any]]
    )

  def apply(row: Int)(column: Int): T =
    matrix(row)(column)

  override def toString: String =
    matrix.map: row =>
      row.mkString + "/"
    .mkString.replaceAll("/$", "")
    
  def order: Int =
    matrix.length

object SquareMatrix:
  def apply[T: ClassTag](values: T*): SquareMatrix[T] =
    val order = intSqrt(values.size)
    val matrix: Array[Array[T]] =
      Array.tabulate(order): row =>
        Array.tabulate(order):column =>
          values:
            (row * order) + column
    new SquareMatrix[T](matrix)
    
  def diagonal[T: ClassTag](
    order: Int, 
    o: T, 
    i: T
  ): SquareMatrix[T] =
    new SquareMatrix[T](
      Array.tabulate(order, order): (m, n) =>
        if m == n then i else o
    )
