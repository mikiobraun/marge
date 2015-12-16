package marge.map

import marge.{Predictor, Learner}
import marge._


/**
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:24 PM
 */

trait OutputMap[A, B] {
  def apply(a: A): B
  def unapply(b: B): Option[A]

  def transform[X](l: Learner[X,B], default: A): Learner[X, A] = new Learner[X,A] {
    def train(ds: Seq[(X, A)]): Predictor[X, A] = {
      val basePredictor = l.train(ds.mapY(a => apply(a)))
      new Predictor[X, A] {
        def apply(x: X): A = {
          unapply(basePredictor(x)).getOrElse(default)
        }
      }
    }
  }
}
