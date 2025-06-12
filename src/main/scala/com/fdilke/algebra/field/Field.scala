package com.fdilke.algebra.field

import com.fdilke.utility.SetsUtilities
import com.fdilke.utility.SetsUtilities.allMaps

import java.io.InputStream
import scala.reflect.ClassTag
import scala.util.matching.Regex

trait Field[T: ClassTag]:
  val O: T
  val I: T
  val elements: Seq[T]
  def dump(): Unit

  def add(element1: T, element2: T): T
  def multiply(element1: T, element2: T): T
  def minus(element: T): T
  def invert(element: T): T

  def subtract(element1: T, element2: T): T =
    add(element1, minus(element2))
  def divide(element1: T, element2: T): T =
    multiply(element1, invert(element2))
  def squareMatrices(order: Int): Iterable[Array[Array[T]]] =
    val indices: Seq[Int] =
      0 until order
    val possibleRows: Iterable[Map[Int, T]] = {
      allMaps(indices, elements)
    }
    allMaps(indices, possibleRows).map:
      (matrix: Map[Int, Map[Int, T]]) =>
      (indices map: i =>
        val row: Map[Int, T] = matrix(i)
        indices.map: j =>
          row(j)
        .toArray)
      .toArray


object FiniteField:

  private val conwayRegex: Regex = 
    "\\[(.*),(.*),\\[(.*)\\]\\],".r

  private val stream: InputStream =
    getClass.getResourceAsStream("CPimport.txt")

  private val lines =
    scala.io.Source.fromInputStream:
      stream
    .getLines

  private val allFields =
    lines.foldLeft(
      Map[NontrivialPrimePower, FiniteField]()
    ):
      (map, line) =>
        if ((line startsWith "[") && (line endsWith "],"))
          val conwayRegex(pp, nn, coefftsCSV) = line
          val pn = NontrivialPrimePower(pp.toInt, nn.toInt)
          val coeffts = coefftsCSV.split(",").map {
            _.toInt
          }
          map + (pn -> new FiniteField(pn, coeffts))
        else map

  def GF: Int => FiniteField =
    case NontrivialPrimePower(p, n) =>
      allFields(NontrivialPrimePower(p, n))
    case _ =>
      throw new IllegalArgumentException

class FiniteField(
  pn: NontrivialPrimePower,
  coeffts: Seq[Int]
) extends Field[Int]:
  override val O: Int = 0
  override val I: Int = 1

  override val elements: Seq[Int] =
    0 until pn.power
  lazy val polyTable: Seq[Seq[Int]] =
    elements map toPoly

  override def dump(): Unit =
    println("Addition:")
    for (j <- elements)
      print(s"${j} ")
    println("")
    for (i <- elements)
      for (j <- elements)
        print(s"${add(i,j)} ")
      println("")
    println("Multiplication:")
    for (j <- elements)
      print(s"${j} ")
    println("")
    for (i <- elements)
      for (j <- elements)
        print(s"${multiply(i,j)} ")
      println("")

  private lazy val additionTable: Seq[Seq[Int]] =
    elements.map: i =>
      val polyI = polyTable(i)
      elements.map: j =>
        val polyJ = polyTable(j)
        fromPoly:
          (polyI zip polyJ) map:
            case (a, b) => (a + b) % pn.p

  override def add(element1: Int, element2: Int): Int =
    additionTable(element1)(element2)

  private lazy val scalarMultiplicationTable: Seq[Seq[Int]] =
    (0 until pn.p) map: i =>
      elements.map: j =>
        val polyJ = polyTable(j)
        fromPoly:
          polyJ map: k =>
            (k * i) % pn.p

  private lazy val minusTable: Seq[Int] =
    scalarMultiplicationTable:
      pn.p - 1

  override def minus(element1: Int): Int =
    minusTable(element1)

  private lazy val x_n =
    fromPoly:
      coeffts.init.map:
        _ * (pn.p - 1) % pn.p

  private lazy val shiftTable: Seq[Int] =
    elements.map: i =>
      polyTable(i) match
        case tail :+ head =>
          additionTable(
            fromPoly(0 +: tail)
          ):
            scalarMultiplicationTable(head)(x_n)

  private lazy val multiplicationTable: Seq[Seq[Int]] =
    elements.map: i =>
      val polyI = polyTable(i)
      elements.map:j =>
        (Seq.iterate(j, pn.n) { j => shiftTable(j) } zip polyI map {
          case (r, s) => scalarMultiplicationTable(s)(r)
        }).foldLeft(O):
          add

  override def multiply(element1: Int, element2: Int): Int =
    multiplicationTable(element1)(element2)

  private lazy val inversionTable: Seq[Int] =
    elements.map: i =>
      elements.find:
        multiplicationTable(i)(_) == I
      .getOrElse(0)

  override def invert(element: Int): Int =
    inversionTable(element)

  private def toPoly(m: Int): Seq[Int] =
    Seq.iterate(m, pn.n):
      _ / pn.p
    .map:
      _ % pn.p

  private def fromPoly(poly: Seq[Int]): Int =
    poly.foldRight(0):
      (a, b) => a + pn.p * b

case class NontrivialPrimePower(p: Int, n: Int):
  val power: Int =
    Seq.fill(n)(p).product

object NontrivialPrimePower:
  def unapply(n: Int): Option[(Int, Int)] =
    if n < 1 then
      None
    else
      (2 to n).find:
        d => 0 == n % d
      .flatMap: p =>
        def logP: Int => Option[Int] =
          case 1 => Some(0)
          case pr if pr % p == 0 => logP(pr/p).map { _ + 1 }
          case _ => None
        logP(n) map:
          r => p -> r
