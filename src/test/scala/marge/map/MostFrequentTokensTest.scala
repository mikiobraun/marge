package marge.map

/**
 *
 * User: mikio
 * Date: 4/14/11
 * Time: 2:25 PM
 */

import org.junit._
import Assert._

class MostFrequentTokensTest {
  @Test
  def simple() {
    val base = Seq("a a a a a b b b b c c c d d e f g h i j k l m n")
    val f = new MostFrequentTokens[String,String](3, base, (x => x.split(" ")))
    println(f.features)
    println(f("a b c d e f g c c c c"))
  }
}