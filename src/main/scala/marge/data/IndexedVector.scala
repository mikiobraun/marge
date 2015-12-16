package marge.data

/**
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:21 PM
 */

trait IndexedVector[S] {
  def apply(i: S): Double
  def length: Int
  def keys: Iterable[S]
}
