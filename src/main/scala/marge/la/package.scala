package marge

import scala.language.implicitConversions

package object la {
  implicit def double2Scalar(d: Double): Scalar = new Scalar(d)
  implicit def int2Scalar(i: Int): Scalar = new Scalar(i.toDouble)

  def log10[E](x: Vector[E]): Vector[E] = x.mapElements(scala.math.log10)
}
