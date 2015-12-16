package marge.map

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:26 PM
 */

class OneAgainstRest[A](positive: A) extends OutputMap[A, Int] {
  def apply(a: A): Int = if (a == positive) 1 else -1
  def unapply(b: Int): Option[A] = if (b == 1) Some(positive) else None
}
