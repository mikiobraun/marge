package marge.map

import scala.math.round

/**
 * <one line description>
 *
 * <longer description>
 *
 * User: mikio
 * Date: 3/29/12
 * Time: 4:29 PM
 */

class ClassToIndexMap[Y](classes: Set[Y])(implicit ord: Ordering[Y]) extends OutputMap[Y, Double] {
  val cl: Seq[Y] = classes.toSeq.sorted

  val classToIndex: Map[Y, Int] = cl.zipWithIndex.toMap

  def indexToClass(z: Double): Option[Y] = {
    val a = round(z).toInt
    if (a < 0 || a >= cl.length) {
      None
    } else {
      Some(cl(a))
    }
  }

  def apply(a: Y): Double = classToIndex(a).toDouble

  def unapply(b: Double): Option[Y] = indexToClass(b)
}
