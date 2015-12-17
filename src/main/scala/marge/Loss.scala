package marge

/**
 * Loss function
 *
 * Actually, this isn't the classical loss function (defined between two labels), but already the
 * function which takes two label sequences and computes the result. That way, you have a bit more flexibility.
 *
 *
 * User: mikio
 * Date: 4/8/11
 * Time: 3:58 PM
 */
trait Loss[Y] extends ((Seq[Y], Seq[Y]) => Double)

object Loss {
  private def checkLabelLengths[Y](y1: Seq[Y], y2: Seq[Y]) {
    if (y1.length != y2.length) {
      throw new IllegalArgumentException("Label sequences have different length (%d != %d)".format(y1.length, y2.length))
    }
  }

  /**
   * ZeroOne loss, computes the fraction of entries which are different in both.
   *
   * @tparam Y
   * @return
   */
  def zeroOne[Y] = new Loss[Y] {
    def apply(y1: Seq[Y], y2: Seq[Y]): Double = {
      checkLabelLengths(y1, y2)
      y1.zip(y2).count(y => y._1 != y._2).toDouble / y1.length
    }
  }

  def zeroOneSignum = new Loss[Double] {
    def apply(y1: Seq[Double], y2: Seq[Double]): Double = {
      checkLabelLengths(y1, y2)
      y1.zip(y2).count(y => sign(y._1) != sign(y._2)).toDouble / y1.length
    }
  }

  def sign(x: Double): Double =
    if (x > 0.0)
      1.0
    else if (x < 0.0)
      -1.0
    else 0.0

  /**
   * Helper function to sum the output of some function over the whole sequence.
   *
   * @param y1
   * @param y2
   * @param fct
   * @return
   */
  def sum(y1: Seq[Double], y2: Seq[Double])(fct: (Double, Double) => Double): Double = {
    checkLabelLengths(y1, y2)
    y1.zip(y2).map(ys => fct(ys._1, ys._2)).sum
  }

  /**
   * Helper function to compute the mean of some function over the two sequences.
   * @param y1
   * @param y2
   * @param fct
   * @return
   */
  def mean(y1: Seq[Double], y2: Seq[Double])(fct: (Double, Double) => Double): Double = {
    sum(y1, y2)(fct) / y1.length
  }

  /**
   * Check whether a and b have the same sign.
   *
   * @param a
   * @param b
   * @return
   */
  def sameSign(a: Double, b: Double): Double = if (a * b > 0.0) 1.0 else 0.0

  /**
   * Zero-one function two two classes.
   */
  val zeroOneTwoClass = new Loss[Double] {
    def apply(y1: Seq[Double], y2: Seq[Double]): Double = mean(y1, y2)((a, b) => sameSign(a, b))
  }
}
