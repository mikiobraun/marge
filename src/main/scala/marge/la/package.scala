package marge

package object la {
  implicit def double2Scalar(d: Double) = new Scalar(d)
  implicit def int2Scalar(i: Int) = new Scalar(i.toDouble)

  def log10[E](x: Vector[E]): Vector[E] = x.mapElements(scala.math.log10 _)
}
