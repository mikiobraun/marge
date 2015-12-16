package marge

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/11/11
 * Time: 2:58 PM
 */

class SeqDoubleFunctions(seq: Seq[Double]) {
}

object SeqDoubleFunctions {
  def mean(s: Seq[Double]): Double = s.sum / s.length

  def argmax(s: Seq[Double]): Int = s.zipWithIndex.sortBy(_._1).last._2
}
