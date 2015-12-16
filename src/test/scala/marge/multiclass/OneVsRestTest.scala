package marge.multiclass

/**
 * <one line description>
 *
 * <longer description>
 *
 * User: mikio
 * Date: 3/21/12
 * Time: 6:46 PM
 */

import org.junit._
import Assert._
import marge.{Predictor, Learner}

class OneVsRestTest {
  @Test
  def checkingTheSubsets() {
    val l = new Learner[String,Double] {
      def train(ds: Seq[(String, Double)]): Predictor[String, Double] = {
        println("Train set: " + ds.mkString(", "))
        new Predictor[String,Double] {
          def apply(x: String): Double = 0.0
        }
      }
    }

    val ds = Seq(
      ("hello", "x"),
      ("you", "y"),
      ("world", "z")
    )

    new OneVsRestLearner[String,String](l, 0.0, "-").train(ds)
  }
}
