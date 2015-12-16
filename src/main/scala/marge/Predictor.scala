package marge

import java.io.{FileOutputStream, BufferedOutputStream, ObjectOutputStream}


/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:27 PM
 */

trait Predictor[X, Y] extends ((X) => Y) {
  def apply(x: X): Y

  def applyAll(xs: Seq[X]): Seq[Y] = xs.map(x => this(x))

  def preprocess[A](in: (A) => X) = new PredictorWithPreprocessing[X, Y, A](this, in)

  def save(out: ObjectOutputStream) {}

  def save(fn: String) {
    val out = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(fn)))
    save(out)
    out.close()
  }
}

object Predictor {
  def fct2predictor[X, Y](fct: (X) => Y) = new FunctionPredictor(fct)
}

class FunctionPredictor[X, Y](fct: (X) => Y) {
  def apply(x: X): Y = fct(x)
}

class PredictorWithPreprocessing[X, Y, A](pred: Predictor[X, Y], in: (A) => X) extends Predictor[A, Y] {
  def apply(a: A): Y = pred(in(a))
}
