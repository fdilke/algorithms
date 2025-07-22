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

  override def hashCode(): Int =
    util.Arrays.deepHashCode(matrix.asInstanceOf[Array[Any]])

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

  def this(values: T*)(implicit tag: ClassTag[T]) =
    this(SquareMatrix.fromSquare(values))

object SquareMatrix:
  def fromSquare[T: ClassTag](values: Seq[T]): Array[Array[T]] =
    val order = intSqrt(values.size)
    Array.tabulate(order): row =>
      Array.tabulate(order):column =>
        values:
          (row * order) + column

  def diagonal[T: ClassTag](
    order: Int, 
    o: T, 
    i: T
  ): SquareMatrix[T] =
    SquareMatrix[T]:
      Array.tabulate(order, order): (m, n) =>
        if m == n then i else o
