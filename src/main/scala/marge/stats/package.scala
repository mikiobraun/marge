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
  def dirichlet(x: Seq[Double], alpha: Seq[Double]): Double = {
    var result = 1.0 / beta(alpha)
    for (i <- alpha.indices) {
      result *= pow(x(i), alpha(i))
    }
    result
  }

  def beta(alpha: Seq[Double]): Double = {
    prod(alpha.map(gamma)) / gamma(sum(alpha))
  }

  def gamma(x: Double): Double = exp(org.apache.commons.math.special.Gamma.logGamma(x))

  def pgaussian(mean: Double, variance: Double): (Double) => Double = { x: Double =>
    exp(- (x - mean)*(x-mean) / variance) / math.sqrt(2*math.Pi*variance)
  }
}
