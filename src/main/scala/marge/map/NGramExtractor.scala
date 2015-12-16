package marge.map

import collection.mutable.HashMap
import marge.data.SparseVector

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:25 PM
 */

class NGramExtractor(n: Int) extends ((String) => Map[String, Int]) {
  def apply(s: String): Map[String, Int] = {
    val counts = new HashMap[String, Int]
    for (k <- n to n) {
      for (i <- 0 until s.length - k) {
        val sub = s.substring(i, i + k)
        val c = counts.getOrElse(sub, 0)
        counts(sub) = c + 1
      }
    }
    counts.toMap
  }
}

class SparseNGramExtractor(val n: Int, kd: KeyDictionary[String]) extends ((String) => SparseVector) {
  val ngram = new NGramExtractor(n)

  def apply(s: String): SparseVector = SparseVector(ngram(s).map(kv => (kd.getKey(kv._1), kv._2.toDouble)))
}
