package marge.la

trait VectorSpace[E] {
  def make(s: Iterable[E]): Vector[E]

  def make(s: Set[E]): Vector[E]

  def make[S](s: Map[E, S])(implicit num: Numeric[S]): Vector[E]

  def make[S](s: Iterable[(E, S)])(implicit num: Numeric[S]): Vector[E]

  def zero = make(Seq[E]())
}

object VectorSpace {
  def apply[E](implicit m: Manifest[E]) {

  }
}
