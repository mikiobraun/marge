package marge

import marge.SeqDoubleFunctions.mean

/**
 *
 * User: mikio
 * Date: 4/8/11
 * Time: 4:03 PM
 */

class CrossValidation[X, Y](k: Int, data: Seq[(X, Y)]) {
  val n = data.length
  val p = CrossValidation.partition(k, n)
  val all = 0 until n

  def evaluate[E](fct: (Seq[(X, Y)], Seq[(X, Y)]) => E): Seq[E] = {
    (0 until k).map {
      i =>
        val testIdx = p(i)
        val trainIdx = all diff testIdx

        fct(subset(trainIdx), subset(testIdx))
    }
  }

  def validate(l: Learner[X, Y], loss: Loss[Y]): Seq[Double] = {
    evaluate {
      (tr, te) =>
        val p = l.train(tr)
        val yhat = p.applyAll(te.xs)
        loss(te.ys, yhat)
    }
  }

  def subset(indices: Seq[Int]): Seq[(X, Y)] = indices.map(i => data(i))
}

object CrossValidation {
  def partition(k: Int, n: Int): Seq[Seq[Int]] = {
    (1 to k).map {
      i =>
        val begin = (i - 1) * n / k
        val end = i * n / k
        begin until end
    }
  }
}

class CrossValidatedLearner[P, X, Y](k: Int, params: Seq[P], loss: Loss[Y], learner: (P) => Learner[X, Y]) extends Learner[X, Y] {
  def train(ds: Seq[(X, Y)]): Predictor[X, Y] = {
    val cv = new CrossValidation(k, ds)
    println("## Running %d-fold CV for params = (%s)".format(k, params.mkString(", ")))
    val errors = params.map {
      p =>
        val saved = System.nanoTime
        println("## cross-validating parameter %s...".format(p))
        val result = (p, mean(cv.validate(learner(p), loss)))
        println("##   error = %f".format(result._2))
        println("##   training time = %.1fs".format((System.nanoTime - saved) / 1e9))
        result
    }
    val bestP = errors.minBy(_._2)._1

    println("## using best parameter: " + bestP)

    learner(bestP).train(ds)
  }
}
