package marge.map

import marge.data.SparseVector

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/14/11
 * Time: 2:36 PM
 */

class MapToSparseVector[X](kd: KeyDictionary[X]) extends ((Map[X, Int]) => SparseVector) {
  def apply(m: Map[X, Int]): SparseVector = SparseVector(m.map(kv => (kd.getKey(kv._1), kv._2.toDouble)))
}
