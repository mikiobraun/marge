package marge.map

/**
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:24 PM
 */

trait InputMap[A,B] {
  def apply(a: A): B
}
