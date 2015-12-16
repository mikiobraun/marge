package marge

/**
 *
 * User: mikio
 * Date: 4/11/11
 * Time: 2:18 PM
 */

import data.{Row, Matrix}
import org.junit._
import Assert._
import marge._

class MargeTest {
  @Test
  def testPermute() {
    val s = (1 to 10)
    println(s.zip(s).permute())
    println(s.zip(s).permute())
  }

  @Test
  def testSubsample() {
    val s = 1 to 10
    for (i <- 1 to 10)
      println(s.zip(s).subsample(i))
  }

  @Test
  def testConfusionMatrix() {
    val y1 = List("a", "b", "c", "a")
    val y2 = List("a", "a", "b", "a")

    val cm = confusionMatrix(y1, y2)
    println(confusionMatrix(y1, y2))
    val exp = Matrix((2.0,0.0,0.0),(1.0,0.0,0.0),(0.0,1.0,0.0))
    assertEquals(exp, cm._1)
  }
}