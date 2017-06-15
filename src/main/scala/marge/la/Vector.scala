package marge.la

import scala.math.{pow,abs}

/**
 * Sort of generic vector of finite index spaces
 *
 * You need to implement at least space, apply, values, and support.
 *
 * User: mikio
 * Date: 9/5/11
 * Time: 11:13 AM
 * To change this template use File | Settings | File Templates.
 */
trait Vector[E] {
  def space: VectorSpace[E]

  /**
   * Get the value for index e
   */
  def apply(e: E): Double

  /**
   * Get the values (non-zero entries) in no particular order.
   */
  def values: Iterable[Double]

  /**
   * Get the indices for the non-zero entries in no particular order.
   */
  def support: Iterable[E]

  def jointSupport(other: Vector[E]): Iterable[E] = (support ++ other.support).toSet

  def +(other: Vector[E]): Vector[E] = space.make(jointSupport(other).map(e => (e, this(e) + other(e))))

  def -(other: Vector[E]): Vector[E] = space.make(jointSupport(other).map(e => (e, this(e) + other(e))))

  def *(scalar: Double): Vector[E] = space.make(support.map(e => (e, this(e) * scalar)))

  def /(scalar: Double): Vector[E] = space.make(support.map(e => (e, this(e) / scalar)))

  //def /:(scalar: Double): Vector[E] = space.make(support.map(e => (e, scalar / this(e))))

  def *!(other: Vector[E]): Vector[E] = space.make(jointSupport(other).map(e => (e, this(e) * other(e))))

  def /!(other: Vector[E]): Vector[E] = space.make(jointSupport(other).map(e => (e, this(e) / other(e))))

  def norm(p: Double=2.0): Double = pow(values.map(v => pow(abs(v), p)).sum, 1/p)

  def normalize(p: Double=2.0): Vector[E] = { val n = norm(p); if (n > 0.0) this / norm(p) else this }

  def length: Int = support.size

  /**
   * Scalar product
   */
  def <*>(other: Vector[E]): Double = support.map(e => this(e) * other(e)).sum

  def sortBy(fct: (E) => Double): Seq[E] = support.toSeq.sortBy(fct)

  def sortWith(cmp: (E, E) => Boolean): Seq[E] = support.toSeq.sortWith(cmp)

  def sortedDown: Seq[E] = support.toSeq.sortBy(e => -this(e))

  def sortedUp: Seq[E] = support.toSeq.sortBy(e => this(e))

  def toSeq: Seq[(E, Double)] = support.map(e => (e, this(e))).toSeq

  def toMap: Map[E,Double] = support.map(e => (e, this(e))).toMap

  def map[F](fct: E => Iterable[F])(implicit fs: VectorSpace[F]): Vector[F] =
    support.foldLeft(fs.zero)((f, e) => f + this(e) * fs.make(fct(e)))

  def mapElements(fct: (Double) => Double): Vector[E] = space.make(support.map(e => (e, fct(this(e)))))

  def top(n: Int): Vector[E] = space.make(sortedDown.map(e => (e, this(e))).take(n))

  def filter(fct: ((E, Double)) => Boolean): Vector[E] = space.make(support.map(e => (e, this(e))).filter(fct))

  /* The usual stuff */

  override def toString: String =
    "(" + support.map(e => "%s=%f".format(e, this(e))).mkString(", ") + ")"

  override def equals(a: Any): Boolean = a match {
    case o: Vector[_] =>
      val other = o.asInstanceOf[Vector[E]]
      support == other.support && support.forall(e => this(e) == other(e))
    case _ => false
  }
}
