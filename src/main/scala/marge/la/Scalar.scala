package marge.la

class Scalar(d: Double) {
  def *[E](v: Vector[E]): Vector[E] = v * d
  def /[E](v: Vector[E]): Vector[E] = v * 1/d
}

object Scalar {
  def apply(d: Double) = new Scalar(d)
}
