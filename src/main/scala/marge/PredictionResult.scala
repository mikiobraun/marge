package marge

import data.Matrix

/**
 * Prediction result - class to store prediction results
 *
 * A class which stores a sequence of labels: the true ones and the predicted ones
 * and then computes all kinds of error measures based on this.
 *
 * User: mikio
 * Date: 3/14/12
 * Time: 2:32 PM
 */
class PredictionResult[Y](trueYSeq: Seq[Y], predictedYSeq: Seq[Y])(implicit manifest: Manifest[Y]) {
  private val trueY = trueYSeq.toArray
  private val predictedY = predictedYSeq.toArray

  if (trueY.length != predictedY.length) {
    throw new IllegalArgumentException("true and predicted labels must have same lengths!")
  }

  val size = trueY.length
  val indices = (0 until trueY.length).toArray
  val classes: Set[Y] = trueY.toSet ++ predictedY.toSet

  def meanLoss(lossFct: (Y,Y) => Double): Double =
    indices.map(i => lossFct(trueY(i), predictedY(i))).sum / size

  def confusionMatrix(implicit ord: Ordering[Y]): Matrix[Int] = {
    val (m, map) = marge.confusionMatrix(trueY, predictedY)
    val labels = (0 until map.size).map(i => map(i).toString)
    m.colNames = labels
    m.rowNames = labels
    m
  }

  def truePositives(c: Y): Int = indices.count(i => trueY(i) == c && predictedY(i) == c)

  def falsePositives(c: Y): Int = indices.count(i => trueY(i) != c && predictedY(i) == c)

  def trueNegatives(c: Y): Int = indices.count(i => trueY(i) != c && predictedY(i) != c)

  def falseNegatives(c: Y): Int = indices.count(i => trueY(i) == c && predictedY(i) != c)

  def positives(c: Y): Int = indices.count(i => trueY(i) == c)

  def negatives(c: Y): Int = indices.count(i => trueY(i) != c)

  def truePositiveRate(c: Y): Double = truePositives(c).toDouble / positives(c)

  def falsePositiveRate(c: Y): Double = falsePositives(c).toDouble / negatives(c)

  def precision(c: Y): Double = {
    val tp = truePositives(c)
    val fp = falsePositives(c)
    tp.toDouble / (tp + fp)
  }

  def recall(c: Y): Double = {
    val tp = truePositives(c)
    val fn = falseNegatives(c)
    tp.toDouble / (tp + fn)
  }

  def precisionAndRecall: Map[Y, (Double, Double)] = {
    var result = Map[Y, (Double, Double)]()
    for (c <- classes) {
      result += c -> ((precision(c), recall(c)))
    }
    result
  }
}

trait LossFct[Y] extends ((Y, Y) => Double)
