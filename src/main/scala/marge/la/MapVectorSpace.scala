package marge.la


class MapVectorSpace[E] extends VectorSpace[E] {
  def make(s: Iterable[E]) = {
    var d = Map[E, Double]()
    for (e <- s) {
      d += e -> (d.getOrElse(e, 0.0) + 1)
    }
    new MapVector(this, d)
  }

  def make(s: Set[E]) = new MapVector(this, s.map(e => (e, 1.0)).toMap)

  def make[S](s: Map[E, S])(implicit num: Numeric[S]) = new MapVector(this, s.map(es => (es._1, num.toDouble(es._2))))

  def make[S](s: Iterable[(E, S)])(implicit num: Numeric[S]) = {
    var d = Map[E, Double]()
    for ((e, v) <- s) {
      d += e -> (d.getOrElse(e, 0.0) + num.toDouble(v))
    }
    new MapVector(this, d)
  }
}
