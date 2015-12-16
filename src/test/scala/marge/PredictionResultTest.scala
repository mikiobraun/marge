package marge

/**
 * <one line description>
 *
 * <longer description>
 *
 * User: mikio
 * Date: 3/14/12
 * Time: 4:21 PM
 */

import org.junit._
import Assert._
import util.Random

class PredictionResultTest {
  @Test
  def confusionMatrix() {
    val t = Seq(1, 2, 1, 3, 1, 4, 1, 5)
    val p = Seq(1, 1, 1, 1, 1, 1, 1, 1)

    val cm = new PredictionResult(t, p).confusionMatrix

    assertEquals(Seq(1, 2, 3, 4, 5).map(_.toString), cm.rowNames)
    assertEquals(Seq(1, 2, 3, 4, 5).map(_.toString), cm.colNames)

    println(cm)
  }

  @Test
  def kindsOfErrors() {
    val t = Seq(0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
    val p = Seq(0, 1, 1, 0, 0, 0, 1, 1, 1, 1)

    val pr = new PredictionResult(t, p)

    assertEquals(7, pr.positives(1))
    assertEquals(3, pr.negatives(1))
    assertEquals(4, pr.truePositives(1))
    assertEquals(1, pr.trueNegatives(1))
    assertEquals(2, pr.falsePositives(1))
    assertEquals(3, pr.falseNegatives(1))

  }

  @Test
  def precisionRecall() {
    val t = Seq(1, 0, 1, 1, 1)
    val p = Seq(1, 1, 0, 0, 0)

    val pr = new PredictionResult(t, p)

    assertEquals(0.5, pr.precision(1), 1e-3)
    assertEquals(0.25, pr.recall(1), 1e-3)
  }

  /**
   * Large results seem to take forever to compare, let's find out why...
   */
  @Test
  def largeResults() {
    val n = 100000
    val t = randClasses(n, 10)
    val p = randClasses(n, 10)

    val pt = new PredictionResult(t, p)
    tictoc("computing it") {
      pt.precisionAndRecall
    }
  }

  def tictoc[T](msg: String)(block: => T): T = {
    print(msg); System.out.flush()
    val savedTime = System.nanoTime
    val result = block
    println(" (%.1fs)".format((System.nanoTime - savedTime) / 1e9))
    result
  }


  def randClasses(n: Int, k: Int): Seq[Int] = {
    val c = new Array[Int](n)
    var i = 0
    while (i < n) {
      c(i) = Random.nextInt(k)
      i += 1
    }
    c.toList
  }
}
