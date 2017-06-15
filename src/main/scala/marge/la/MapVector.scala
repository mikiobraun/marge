package marge.la

class MapVector[E](vs: MapVectorSpace[E], data: Map[E, Double]) extends Vector[E]{
  def space: MapVectorSpace[E] = vs

  def apply(e: E): Double = data.getOrElse(e, 0.0)

  def values: Iterable[Double] = data.values

  def support: Iterable[E] = data.keys
}
