package marge.stats

/**
 * <one line description>
 *
 * <longer description>
 *
 * User: mikio
 * Date: 1/30/12
 * Time: 12:26 PM
 */

import org.junit._
import Assert._

class StatTests {
  @Test
  def testGamma() {
    val valuesOfGamma = Seq[Double](1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880)

    for (i <- 1 to valuesOfGamma.length) {
      assertEquals(valuesOfGamma(i-1), gamma(i.toDouble), 1e-5)
    }

    assertEquals(1.32934, gamma(2.5), 1e-5)
  }

  def testDirichlet() {

  }
}