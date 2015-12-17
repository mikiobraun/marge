package marge.liblinear

import marge.data.SparseVector
import marge.{Predictor, Learner}
import de.bwaldvogel.liblinear._
import java.io._

/**
 * <one line description>
 *
 * <longer description>
 *
 * User: mikio
 * Date: 3/20/12
 * Time: 4:13 PM
 */

class LibLinearJavaLearner(c: Double) extends Learner[SparseVector, Double] {
  val solver = SolverType.MCSVM_CS

  def buildProblem(ds: Seq[(SparseVector, Double)]): Problem = {
    val problem = new Problem
    problem.l = ds.length
    problem.n = ds.map(_._1.length).max
    problem.y = ds.map(_._2.toInt).toArray
    problem.x = new Array[Array[FeatureNode]](ds.length)
    //problem.bias = 1.0a
    var i = 0
    while (i < problem.l) {
      problem.x(i) = LibLinearJava.sparseToFeatures(ds(i)._1)
      i += 1
    }
    problem
  }


  def train(ds: Seq[(SparseVector, Double)]): Predictor[SparseVector, Double] = {
    println("Building problem")
    val problem = buildProblem(ds)
    val param = new Parameter(solver, c, 1e-3)
    //Linear.disableDebugOutput()
    Linear.enableDebugOutput()
    println("training")
    new LibLinearJavaPredictor(Linear.train(problem, param))
  }
}

object LibLinearJava {
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
}

class LibLinearJavaPredictor(model: Model) extends Predictor[SparseVector, Double] {
  private val labels = model.getLabels

  def apply(x: SparseVector): Double = {
    val out = new Array[Double](2)
    //Linear.predictValues(model, LibLinearJava.sparseToFeatures(x), out)
    //println(out.toList)
    //labels(0) * out(0)
    Linear.predict(model, LibLinearJava.sparseToFeatures(x))
  }

  override def save(out: ObjectOutputStream) {
    val buffer = new ByteArrayOutputStream()
    Linear.saveModel(new BufferedWriter(new OutputStreamWriter(buffer)), model)
    out.writeObject(buffer.toByteArray)
  }
}

object LibLinearJavaPredictor {
  def load(in: ObjectInputStream): LibLinearJavaPredictor = {
    val buffer = in.readObject().asInstanceOf[Array[Byte]]
    val model = Linear.loadModel(new BufferedReader(new InputStreamReader(new ByteArrayInputStream(buffer))))
    new LibLinearJavaPredictor(model)
  }
}
