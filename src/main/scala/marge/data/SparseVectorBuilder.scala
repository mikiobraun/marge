package marge.data

import collection.mutable.{ArrayBuffer, Builder}

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:25 PM
 */

class SparseVectorBuilder extends Builder[(Int, Double), SparseVector] {
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
