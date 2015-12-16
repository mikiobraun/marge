package marge

import marge._
import scala.math.{exp,pow}

package object stats {
  /**
   * Dirichlet distribution
   * @param x
   * @param alpha
   * @return
   */
  def dirichlet(x: Seq[Double], alpha: Seq[Double]) = {
    var result = 1.0 / beta(alpha)
    for (i <- 0 until alpha.length) {
      result *= pow(x(i), alpha(i))
    }
    result
  }

  def beta(alpha: Seq[Double]): Double = {
    prod(alpha.map(gamma _)) / gamma(sum(alpha))
  }

  def gamma(x: Double) = exp(org.apache.commons.math.special.Gamma.logGamma(x))

  def pgaussian(mean: Double, variance: Double) = { x: Double =>
    exp(- (x - mean)*(x-mean) / variance) / math.sqrt(2*math.Pi*variance)
  }
}
