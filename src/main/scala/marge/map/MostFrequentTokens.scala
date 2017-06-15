package marge.map

import collection.mutable.HashMap
import marge._

import scala.collection.mutable

/**
 *
 * User: mikio
 * Date: 4/14/11
 * Time: 2:19 PM
 */

class MostFrequentTokens[X, Z](n: Int, base: Seq[X], tokenizer: (X) => Seq[Z]) extends ((X) => Map[Z, Int]) {
  def findTopTokens(): Map[Z, Int] = {
    val h = new mutable.HashMap[Z, Int]

    def update(z: Z) {
      val count = h.getOrElse(z, 0)
      h(z) = count + 1
    }

    for (x <- base) {
      tokenizer(x).foreach(update)
    }

    h.toList.sortBy(kv => -kv._2).take(n).toMap
  }

  val features: Map[Z, Int] = findTopTokens()

  def apply(x: X): Map[Z, Int] = hist(tokenizer(x).filter(z => features.contains(z)))
}
