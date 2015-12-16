package marge.la

class MapVector[E](vs: MapVectorSpace[E], data: Map[E, Double]) extends Vector[E]{
  def space = vs

  def apply(e: E) = data.getOrElse(e, 0.0)

  def values = data.values

  def support = data.keys
}
