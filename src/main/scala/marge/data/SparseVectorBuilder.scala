package marge.data

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:25 PM
 */

class SparseVectorBuilder extends mutable.Builder[(Int, Double), SparseVector] {
  val indices = new ArrayBuffer[Int]
  val values = new ArrayBuffer[Double]

  def +=(elem: (Int, Double)): SparseVectorBuilder.this.type = {
    indices.append(elem._1)
    values.append(elem._2)
    this
  }

  def result(): SparseVector = null

  def clear() {}
}
