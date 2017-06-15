package marge.multiclass

import marge.SeqDoubleFunctions._
import marge._
import java.io.{ObjectInputStream, ObjectOutputStream}

/**
 *
 * User: mikio
 * Date: 4/12/11
 * Time: 2:58 PM
 */

class OneVsRestLearner[X, Y](base: Learner[X, Double], threshold: Double, default: Y)(implicit org: Ordering[Y]) extends Learner[X, Y] {
  def train(ds: Seq[(X, Y)]): OneVsRestPredictor[X, Y] = {
    val classes = ds.classes.toSeq.sorted
    val ps = classes.map {
      cl =>
        println("Training class " + cl)
        val trainData = ds.oneAgainstAll(cl).subsampleSameNumberOfNegativeExamples(1.0)
        //println("TrainData: " + trainData)
        val p = base.train(trainData)
//        locally {
//          val trainLabels = trainData.ys
//          val predictedLabels = p.applyAll(trainData.xs)
//          println("training =  " + trainLabels.map(s => "%+.1f".format(s)))
//          println("predicted = " + predictedLabels.map(s => "%+.1f".format(s)))
//          println("  " + Loss.zeroOneSignum(trainLabels, predictedLabels))
//        }
        p
    }
    new OneVsRestPredictor[X, Y](classes, ps, threshold, default)
  }
}

class OneVsRestPredictor[X, Y](val classes: Seq[Y], val predictors: Seq[Predictor[X, Double]], val threshold: Double, val default: Y) extends Predictor[X, Y] {
  val indices: Range = classes.indices

  def apply(x: X): Y = {
    val scores = predictors.map(_(x))
    //println(classes.zip(scores))
    val winner = argmax(scores)
    if (scores(winner) <= threshold)
      default
    else
      classes(winner)
  }

  def scores(x: X): Seq[(Y, Double)] = indices.map(i => (classes(i), predictors(i)(x)))

  def predictAndScore(x: X): (Y, Double) = {
    val scores = predictors.map(_(x))
    val winner = argmax(scores)
    (classes(winner), scores(winner))
  }

  override def save(out: ObjectOutputStream) {
    out.writeObject(classes)
    out.writeInt(predictors.length)
    for (p <- predictors) {
      p.save(out)
    }
    out.writeDouble(threshold)
    out.writeObject(default)
  }
}

object OneVsRestPredictor {
  def load[X,Y](in: ObjectInputStream, loader: (ObjectInputStream) => Predictor[X, Double]): OneVsRestPredictor[X,Y] = {
    val classes = in.readObject().asInstanceOf[Seq[Y]]
    val numPred = in.readInt()
    val predictors = new Array[Predictor[X, Double]](numPred)
    for (i <- 0 until numPred) {
      predictors(i) = loader(in)
    }
    val threshold = in.readDouble()
    val default = in.readObject().asInstanceOf[Y]
    new OneVsRestPredictor(classes, predictors, threshold, default)
  }
}
