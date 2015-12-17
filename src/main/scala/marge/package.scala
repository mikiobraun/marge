import marge.data.Matrix
import math.pow
import java.util.Random
import scala.language.implicitConversions

package object marge {
  def linspace(a: Double, b: Double, n: Int): Seq[Double] =
    (0 until n).map(i => a + (b - a) * i.toDouble / (n - 1))

  def logspace(a: Double, b: Double, n: Int): Seq[Double] =
    linspace(a, b, n).map(x => pow(10, x))

  implicit def seq2DataSet[X, Y](seq: Seq[(X, Y)]): DataSetAdditions[X, Y] = new DataSetAdditions[X, Y](seq)

  /**
   * Create a random permutation of the numbers 0, ..., size - 1.
   *
   * see Algorithm P, D.E. Knuth: The Art of Computer Programming, Vol. 2, p. 145
   */
  def randomPermutation(size: Int): Array[Int] = {
    val r: Random = new Random
    val result: Array[Int] = new Array[Int](size)

    var j = 0
    while (j < size) {
      result(j) = j
      j += 1
    }

    j = size - 1
    while (j > 0) {
      val k = r.nextInt(j)
      val temp = result(j)
      result(j) = result(k)
      result(k) = temp
      j -= 1
    }
    result
  }

  def shuffle[E](s: Seq[E]): Seq[E] = {
    val p = randomPermutation(s.length)
    p.map(i => s(i))
  }

  /**
   * Get a random sample of k out of n elements.
   *
   * See Algorithm S, D. E. Knuth, The Art of Computer Programming, Vol. 2, p.142.
   */
  def randomSubset(k: Int, n: Int): Array[Int] = {
    assert((0 < k && k <= n))
    val r = new Random
    var t = 0
    var m = 0
    val result = new Array[Int](k)
    while (m < k) {
      val u = r.nextDouble
      if ((n - t) * u < k - m) {
        result(m) = t
        m += 1
      }
      t += 1
    }
    result
  }

  def hist[T](seq: Iterable[T]): Map[T, Int] = {
    var hist = Map[T, Int]()
    seq.foreach {
      t =>
        val c = hist.getOrElse(t, 0)
        hist += (t -> (c + 1))
    }
    hist
  }

  def confusionMatrix[Y](y1: Seq[Y], y2: Seq[Y])(implicit ord: Ordering[Y]): (Matrix[Int], Map[Int, Y]) = {
    assert(y1.length == y2.length)
    val ys = (y1.toSet ++ y2.toSet).toSeq.sorted.zipWithIndex.toMap
    val n = y1.length
    val m = ys.size
    val r = Matrix[Int](m, m)
    for (i <- 0 until n) {
      r(ys(y1(i)), ys(y2(i))) += 1
    }
    (r, ys.map(kv => (kv._2, kv._1)))
  }

  def prod(a: Seq[Double]) = a.foldLeft(1.0)((p, x) => p * x)

  def sum(a: Seq[Double]) = a.foldLeft(0.0)((s, x) => s + x)

  def rand(n: Int): Array[Double] = {
    val r = new Array[Double](n)
    val rnd = new Random()
    for (i <- 0 until n) {
      r(i) = rnd.nextDouble()
    }
    r
  }

  def randn(n: Int): Array[Double] = {
    val r = new Array[Double](n)
    val rnd = new Random()
    for (i <- 0 until n) {
      r(i) = rnd.nextGaussian()
    }
    r
  }

  def permute[T](s: Array[T])(implicit m: Manifest[T]): Array[T] = {
    val n = s.length
    val r = new Array[T](n)
    val p = randomPermutation(n)
    var i = 0
    while (i < n) {
      r(i) = s(p(i))
      i += 1
    }
    r
  }
}
