package marge.data

import scala.language.implicitConversions

/**
 *
 * User: mikio
 * Date: 4/12/11
 * Time: 11:26 AM
 */

class Matrix[T](val rows: Int, val cols: Int)(implicit m: Manifest[T]) {
  val data = new Array[T](rows * cols)

  private val rowIndices = (0 until rows).toArray
  private val colIndices = (0 until cols).toArray

  def index(i: Int, j: Int): Int = i + j * rows

  def apply(i: Int, j: Int): T = data(index(i, j))

  def update(i: Int, j: Int, v: T) { data(index(i,j)) = v }

  def update(i: Int, v: T) { data(i) = v }

  def apply(i: Int): T = data(i)

  override def toString = format("%10s", "  ", "\n")

  def format(fmt: String, sep: String, newline: String) =
    (0 until rows).map { i =>
      (0 until cols).map { j =>
        fmt.format(this(i,j))
      }.mkString(sep)
    }.mkString(newline)

  override def equals(other: Any) = other match {
    case that: Matrix[_] => this.rows == that.rows && this.cols == that.cols && this.data.sameElements(that.data)
    case _ => false
  }

  def columnSums(implicit num: Numeric[T]): Seq[T] = (0 until rows).map(i => (0 until cols).map(j => this(i,j)).sum)

  var rowNames: Seq[String] = Seq()
  var colNames: Seq[String] = Seq()

  def findRow(s: String) = {
    val i = rowNames.indexOf(s)
    if (i == -1)
      throw new NoSuchElementException("No row with name '%s'".format(s))
    else
      i
  }

  def findCol(s: String) = {
    val j = colNames.indexOf(s)
    if (j == -1)
      throw new NoSuchElementException("No column with name '%s'".format(s))
    else
      j
  }

  def apply(str: String, j: Int): T = apply(findRow(str), j)
  def apply(i: Int, str: String): T = apply(i, findCol(str))
  def apply(row: String, col: String): T = apply(findRow(row), findCol(col))

  def update(row: String, j: Int, t: T) { update(findRow(row), j, t) }
  def update(i: Int, col: String, t: T) { update(i, findCol(col), t) }
  def update(row: String, col: String, t: T) { update(findRow(row), findCol(col), t) }

  def col(j: Int): Seq[T] = rowIndices.map(i => this(i, j))
  def row(i: Int): Seq[T] = colIndices.map(j => this(i, j))
}

class Row[T](val data: Seq[T]) {
  def length = data.length
}

object Row {
  def apply[T](data: T*) = new Row(data)

  implicit def tuple1ToRow[T](t: Tuple1[T]) = new Row(Seq(t._1))
  implicit def tuple2ToRow[T](t: (T, T)) = new Row(Seq(t._1,t._2))
  implicit def tuple3ToRow[T](t: (T, T, T)) = new Row(Seq(t._1,t._2,t._3))
  implicit def tuple4ToRow[T](t: (T, T, T, T)) = new Row(Seq(t._1,t._2,t._3,t._4))
  implicit def tuple5ToRow[T](t: (T, T, T, T, T)) = new Row(Seq(t._1,t._2,t._3,t._4,t._5))
  implicit def tuple6ToRow[T](t: (T, T, T, T, T, T)) = new Row(Seq(t._1,t._2,t._3,t._4,t._6))
  implicit def tuple7ToRow[T](t: (T, T, T, T, T, T, T)) = new Row(Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7))
  implicit def tuple8ToRow[T](t: (T, T, T, T, T, T, T, T)) = new Row(Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8))
  implicit def tuple9ToRow[T](t: (T, T, T, T, T, T, T, T, T)) = new Row(Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9))
  implicit def tuple10ToRow[T](t: Tuple10[T,T,T,T,T,T,T,T,T,T]) = new Row(Seq(t._1,t._2,t._3,t._4,t._5,t._6,t._7,t._8,t._9,t._10))
}

object Matrix {
  def apply[T](rows: Int, cols: Int)(implicit m: Manifest[T]): Matrix[T]  = new Matrix[T](rows, cols)

  def apply[T](data: Row[T]*)(implicit m: Manifest[T]): Matrix[T] = {
    val rows = data.length
    val cols = data.map(_.length).max
    val result = new Matrix[T](rows, cols)
    for (i <- 0 until rows)
      for (j <- 0 until data(i).length)
        result(i, j) = data(i).data(j)
    result
  }
}
