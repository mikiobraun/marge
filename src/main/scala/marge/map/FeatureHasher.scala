package marge.map

import marge.data.SparseVector

/**
  * Created by mibraun on 17/12/15.
  */
class FeatureHasher[X](bits: Int) extends InputMap[Seq[(X, Double)], SparseVector] {
  private val bitmask = (1 << bits) - 1

  override def apply(a: Seq[(X, Double)]): SparseVector =
    SparseVector(a.map(kv => (kv._1.hashCode & bitmask, kv._2))
      .foldLeft(Map[Int, Double]())((b, kv) => b + (kv._1 -> (b.getOrElse(kv._1, 0.0) + kv._2))))
}
