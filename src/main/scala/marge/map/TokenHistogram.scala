package marge.map

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/14/11
 * Time: 2:43 PM
 */

class TokenHistogram[X,Z](tokenizer: (X) => Seq[Z]) extends ((X) => Map[Z, Int]) {
  def apply(x: X): Map[Z, Int] = marge.hist(tokenizer(x))
}
