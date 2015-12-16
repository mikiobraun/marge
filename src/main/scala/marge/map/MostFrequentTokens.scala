package marge.map

import collection.mutable.HashMap
import marge._

/**
 *
 * User: mikio
 * Date: 4/14/11
 * Time: 2:19 PM
 */

class MostFrequentTokens[X, Z](n: Int, base: Seq[X], tokenizer: (X) => Seq[Z]) extends ((X) => Map[Z, Int]) {
  def findTopTokens() = {
    val h = new HashMap[Z, Int]

    def update(z: Z) {
      val count = h.getOrElse(z, 0)
      h(z) = count + 1
    }

    for (x <- base) {
      tokenizer(x).foreach(update _)
    }

    h.toList.sortBy(kv => -kv._2).take(n).toMap
  }

  val features = findTopTokens()

  def apply(x: X): Map[Z, Int] = hist(tokenizer(x).filter(z => features.contains(z)))
}
