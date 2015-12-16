package marge.data

import java.util.Arrays

/**
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:21 PM
 */

class SparseVector(indices: Array[Int], val data: Array[Double]) extends IndexedVector[Int] {
  assert(indices.length == data.length)

  def apply(i: Int): Double = {
    val j = Arrays.binarySearch(indices, i)
    if (j < 0) 0.0 else data(j)
  }

  def valueAt(i: Int): Double = data(i)

  def indexAt(i: Int): Int = indices(i)

  def length = if (indices.isEmpty) 0 else indices.last + 1

  def numEntries = data.length

  def keys = indices

  def sameElements(that: SparseVector) =
    keys.sameElements(keys) && data.sameElements(that.data)

  def support = indices

  def *(other: Array[Double]): Double = {
    var i = 0
    var sum = 0.0
    while (i < indices.length) {
      val j = indices(i)
      if (j >= other.length)
        return sum
      sum += other(indices(i)) * data(i)
      i += 1
    }
    sum
  }

  def *(other: SparseVector): Double = {
    if (other.support.length < support.length) {
      other * this
    } else {
      var sum = 0.0
      var ti = 0
      var oi = 0
      val ts = support
      val os = other.support
      while (ti < ts.length && oi < os.length) {
        val t = ts(ti)
        val o = os(oi)
        if (t == o) {
          sum += valueAt(ti) * other.valueAt(oi)
          oi += 1
          ti += 1
        } else if (t > o) {
          oi += 1
        } else if (t < o) {
          ti += 1
        }
      }
      sum
    }
  }

  def dot(other: SparseVector): Double = this * other

  def *(scalar: Double): SparseVector = map(x => x * scalar)

  def /(scalar: Double): SparseVector = map(x => x * scalar)

  def map(fct: (Double) => Double): SparseVector = new SparseVector(indices, data.map(fct))

  def toMap = indices.zip(data).toMap

  def toSeq = indices.zip(data)

  override def toString =
    "SparseVector(" +
      (0 until keys.length).map(i => "%d -> %f".format(keys(i), data(i))).mkString(", ") + ")"

  def sum: Double = data.sum

  def norm1: Double = data.foldLeft(0.0)((s, x) => s + math.abs(x))

  def norm2: Double = math.sqrt(sqnorm2)

  def sqnorm2: Double = data.foldLeft(0.0)((s, x) => s + x * x)

  def +(other: SparseVector): SparseVector = {
    val rs = jointSupportSize(other)
    val ridx = new Array[Int](rs)
    val rdata = new Array[Double](rs)
    var ti = 0
    var oi = 0
    var ri = 0

    while (ti < support.length && oi < other.support.length) {
      val t = support(ti)
      val o = other.support(oi)
      //println("this: %d:%d other: %d:%d".format(ti, ti, oi, o))
      if (t == o) {
        ridx(ri) = t
        rdata(ri) = valueAt(ti) + other.valueAt(oi)
        ri += 1
        ti += 1
        oi += 1
      } else if (t < o) {
        ridx(ri) = t
        rdata(ri) = valueAt(ti)
        ri += 1
        ti += 1
      } else {
        ridx(ri) = o
        rdata(ri) = other.valueAt(oi)
        ri += 1
        oi += 1
      }
    }

    // Only one of the following two will be active

    while (ti < support.length) {
      ridx(ri) = support(ti)
      rdata(ri) = valueAt(ti)
      ri += 1
      ti += 1
    }

    while (oi < other.support.length) {
      ridx(ri) = other.support(oi)
      rdata(ri) = other.valueAt(oi)
      ri += 1
      oi += 1
    }

    new SparseVector(ridx, rdata)
  }

  def jointSupportSize(other: SparseVector): Int = {
    var ti = 0
    var oi = 0
    var ri = 0
    while (ti < support.length && oi < other.support.length) {
      val t = support(ti)
      val o = other.support(oi)
      //println("this: %d:%d other: %d:%d".format(ti, ti, oi, o))
      if (t == o) {
        ri += 1
        ti += 1
        oi += 1
      } else if (t < o) {
        ri += 1
        ti += 1
      } else {
        ri += 1
        oi += 1
      }
    }
    ri + support.length - ti + other.support.length - oi
  }

  override def equals(o: Any): Boolean = o match {
    case other: SparseVector =>
      sameElements(other)
    case _ => false
  }

  def -(other: SparseVector): SparseVector = {
    val rs = jointSupportSize(other)
    val ridx = new Array[Int](rs)
    val rdata = new Array[Double](rs)
    var ti = 0
    var oi = 0
    var ri = 0

    while (ti < support.length && oi < other.support.length) {
      val t = support(ti)
      val o = other.support(oi)
      //println("this: %d:%d other: %d:%d".format(ti, ti, oi, o))
      if (t == o) {
        ridx(ri) = t
        rdata(ri) = valueAt(ti) - other.valueAt(oi)
        ri += 1
        ti += 1
        oi += 1
      } else if (t < o) {
        ridx(ri) = t
        rdata(ri) = valueAt(ti)
        ri += 1
        ti += 1
      } else {
        ridx(ri) = o
        rdata(ri) = -other.valueAt(oi)
        ri += 1
        oi += 1
      }
    }

    // Only one of the following two will be active

    while (ti < support.length) {
      ridx(ri) = support(ti)
      rdata(ri) = valueAt(ti)
      ri += 1
      ti += 1
    }

    while (oi < other.support.length) {
      ridx(ri) = other.support(oi)
      rdata(ri) = -other.valueAt(oi)
      ri += 1
      oi += 1
    }

    new SparseVector(ridx, rdata)
  }
}

object SparseVector {
  def apply(data: Map[Int, Double]): SparseVector = apply(data.toSeq: _*)

  def apply(data: (Int, Double)*): SparseVector = {
    val sortedData = data.sortBy(_._1)
    new SparseVector(sortedData.map(_._1).toArray, sortedData.map(_._2).toArray)
  }

  def constantValues(n: Int, v: Double): SparseVector = {
    val indices = (0 until n).toArray
    val data = Array.fill(n)(v)
    new SparseVector(indices, data)
  }
}
