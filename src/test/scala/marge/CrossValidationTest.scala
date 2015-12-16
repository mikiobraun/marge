package marge

/**
 *
 * User: mikio
 * Date: 4/8/11
 * Time: 4:34 PM
 */

import org.junit._
import Assert._

class CrossValidationTest {
  @Test
  def testPartition() {
    println(CrossValidation.partition(3, 10))
  }

  @Test
  def testCV() {
    val x = 1 to 10
    val y = List(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
    val data = x.zip(y)
    val cv = new CrossValidation(5, data)
    cv.evaluate { (train, test) =>
      println("train: " + train)
      println("test: " + test)
    }
  }
}