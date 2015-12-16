/*
 * Copyright (c) 2012 TWIMPACT UG (haftungsbeschraenkt). All rights reserved.
 */

package marge.la

import org.junit._
import Assert._

class VectorTest {
  def checkVector(a: Array[Double], v: Vector[Int]) {
    val support = a.zipWithIndex.filter(ai => ai._1 != 0.0).map(_._2).toSet

    assertEquals(support, v.support.toSet)

    for (i <- 0 until a.length) {
      assertEquals(a(i), v(i), 0.0)
    }
  }

  @Test
  def constructFromIterable() {
    val vs = new MapVectorSpace[Int]
    val v = vs.make(Seq(1, 2, 3, 5, 3, 2, 3))

    checkVector(Array(0.0, 1.0, 2.0, 3.0, 0.0, 1.0, 0.0), v)
  }

  @Test
  def constructFromSet() {
    val vs = new MapVectorSpace[Int]
    val v = vs.make(Set(1, 2, 3, 5))

    checkVector(Array(0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 0.0), v)
  }

  @Test
  def constructFromMap() {
    val vs = new MapVectorSpace[Int]
    val v = vs.make(Map(1 -> 1, 2 -> 2, 3 -> 3, 5 -> 1))

    checkVector(Array(0.0, 1.0, 2.0, 3.0, 0.0, 1.0, 0.0), v)
  }

  @Test
  def constructFromPairIterable() {
    val vs = new MapVectorSpace[Int]
    val v = vs.make(Seq(1 -> 1, 2 -> 2, 3 -> 1, 5 -> 1, 3 -> 2))

    checkVector(Array(0.0, 1.0, 2.0, 3.0, 0.0, 1.0, 0.0), v)
  }

  @Test
  def addition() {
    val vs = new MapVectorSpace[Int]
    val x = vs.make(Seq(1 -> 1, 2 -> 1, 3 -> 1))
    val y = vs.make(Seq(2 -> 1, 3 -> 2, 4 -> 4))

    checkVector(Array(0.0, 1.0, 2.0, 3.0, 4.0, 0.0), x+y)
  }

  @Test
  def mapFunctionTest() {
    val intSpace = new MapVectorSpace[Int]
    val stringSpace = new MapVectorSpace[String]

    val x = intSpace.make((0 until 10).map(i => (i,i)))
    val y = x.map(i => Seq("foo", i.toString))(stringSpace)

    println(x)
    println(y)
  }

  @Test
  def elementwiseDivide() {
    val intSpace = new MapVectorSpace[Int]
    val x = intSpace.make(Seq(1 -> 1, 2 -> 1, 3 -> 1))
    val y = intSpace.make(Seq(1 -> 1, 2 -> 2, 3 -> 3))

    println(x /! y)
    println(y /! x)
  }
}