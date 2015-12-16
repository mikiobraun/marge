package marge.map

class TwoClass[A](positive: A, negative: A) extends OutputMap[A, Int] {
  def apply(a: A): Int = if (a == positive) 1 else if (a == negative) -1 else 0
  def unapply(b: Int): Option[A] = if (b == 1) Some(positive) else if (b == -1) Some(negative) else None
}
