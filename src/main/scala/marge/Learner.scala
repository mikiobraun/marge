package marge

import map.OutputMap

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:33 PM
 */

trait Learner[X, Y] {
  def train(ds: Seq[(X,Y)]): Predictor[X, Y]

  def transformInput[A](in: (A) => X) = new Learner[A, Y] {
    def train(ds: Seq[(A, Y)]): Predictor[A, Y] = {
      val p = Learner.this.train(ds.mapX(in))
      p.preprocess(in)
    }
  }
}
