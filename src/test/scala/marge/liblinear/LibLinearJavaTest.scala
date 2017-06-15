package marge.liblinear

/**
 * <one line description>
 *
 * <longer description>
 *
 * User: mikio
 * Date: 3/20/12
 * Time: 3:58 PM
 */

import org.junit._
import Assert._
import de.bwaldvogel.liblinear._
import marge.data.SparseVector
import collection.mutable.ArrayBuffer
import marge._
import marge.Loss

class LibLinearJavaTest {
  def sparseToFeatures(vec: SparseVector): Array[FeatureNode] = {
    val n = vec.numEntries
    val f = new Array[FeatureNode](n)
    //f(n) = new FeatureNode(-1, 0.0)
    var i = 0
    while (i < n) {
      f(i) = new FeatureNode(vec.indexAt(i) + 1, vec.valueAt(i))
      i += 1
    }
    f
  }

  @Test
  def foo() {
    val problem = new Problem
    problem.n = 6
    problem.l = 5
    problem.y = Array(1, -1, 1, -1, -1)
    problem.x = new Array[Array[FeatureNode]](5)
    problem.x(0) = sparseToFeatures(SparseVector(0 -> 1.0, 1 -> 2.0, 5 -> 0.3))
    problem.x(1) = sparseToFeatures(SparseVector(1 -> 1.0, 4 -> 2.0))
    problem.x(2) = sparseToFeatures(SparseVector(0 -> 1.0, 5 -> -1.0))
    problem.x(3) = sparseToFeatures(SparseVector(0 -> 1.0, 5 -> -1.0))
    problem.x(4) = sparseToFeatures(SparseVector(0 -> 1.0, 5 -> -1.0))
    val param = new Parameter(SolverType.L1R_LR, 1.0, 1e-3)
    val model = Linear.train(problem, param)
    println(model)
  }

  /**
   * Does multiple training lead to worse results? Let's check...
   */
  @Test
  def multipleTraining() {
    val ds = generateData(100)

    var firstError = -1.0
    for (_ <- 1 to 10) {
      val l = new LibLinearJavaLearner(1.0)
      val m = l.train(ds)
      val error = Loss.zeroOne(ds.ys, m.applyAll(ds.xs))
      if (firstError == -1) {
        firstError = error
        println("error = " + error)
      }
      else
        assertEquals(firstError, error, 1e-3)
    }
  }

  def generateData(n: Int): Seq[(SparseVector, Double)] = {
    val data = new ArrayBuffer[(SparseVector, Double)]
    val w = new SparseVector(Array(0, 1, 2, 3, 4, 5), marge.randn(6))
    for (_ <- 1 to n) {
      val x = new SparseVector(Array(0, 1, 2, 3, 4, 5), marge.rand(6))
      data.append((x, scala.math.signum(w * x + scala.util.Random.nextGaussian())))
    }
    data.result()
  }
}
