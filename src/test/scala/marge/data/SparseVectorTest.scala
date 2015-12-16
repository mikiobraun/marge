package marge.data

/**
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 4:04 PM
 */

import org.junit._
import Assert._

class SparseVectorTest {
  @Test
  def testToString() {
    val a = new SparseVector(Array(1,3,11), Array(1.0,2.0,3.0))
    println(a.toString)
  }

  @Test
  def basicAccess() {
    val a = new SparseVector(Array(1,3,11), Array(1.0,2.0,3.0))
    assertEquals(12,a.length)
    assertEquals(0.0, a(0), 1e-9)
    assertEquals(1.0, a(1), 1e-9)
    assertEquals(0.0, a(2), 1e-9)
    assertEquals(2.0, a(3), 1e-9)
    assertEquals(0.0, a(4), 1e-9)
    assertEquals(3.0, a(11), 1e-9)
    assertEquals(0.0, a(12), 1e-9)
  }

  @Test
  def constructFromSeqOfTuples() {
    val a = SparseVector(1 -> 1.0, 3 -> 2.0, 11 -> 3.0)
    val b = new SparseVector(Array(1,3,11), Array(1.0,2.0,3.0))
    assertTrue(a.sameElements(b))
  }

  @Test
  def sparseVectorScalarProduct() {
    assertEquals(14.0, SparseVector(0->1.0, 1->2.0, 3->3.0) * SparseVector(0->1.0, 1->2.0,3->3.0), 1e-6)
    assertEquals(10.0, SparseVector(0->1.0, 1->2.0, 3->3.0) * SparseVector(0->1.0, 2->2.0,3->3.0), 1e-6)
    assertEquals(0.0, SparseVector(1->1.0,3->3.0,5->4.0) * SparseVector(2->2.0, 4->4.0), 1e-6)
  }

  @Test
  def testJointSupport() {
    val x = SparseVector(0 -> 1.0, 1 -> 2.0, 3 -> 3.0)
    val y = SparseVector(4 -> 1.0, 5 -> 1.0, 6 -> 1.0)
    val z = SparseVector(0 -> 1.0, 4 -> 1.0, 8 -> 1.0)

    assertEquals(3, x.jointSupportSize(x))
    assertEquals(6, x.jointSupportSize(y))
    assertEquals(6, y.jointSupportSize(x))
    assertEquals(5, x.jointSupportSize(z))
    assertEquals(5, z.jointSupportSize(x))
    assertEquals(5, y.jointSupportSize(z))
    assertEquals(5, z.jointSupportSize(y))
  }

  @Test
  def testAdd() {
    val x = SparseVector(0 -> 1.0, 1 -> 2.0, 3 -> 3.0)
    val y = SparseVector(4 -> 1.0, 5 -> 1.0, 6 -> 1.0)
    val z = SparseVector(0 -> 1.0, 4 -> 1.0, 8 -> 1.0)

    assertEquals(SparseVector(0 -> 1.000000, 1 -> 2.000000, 3 -> 3.000000, 4 -> 1.000000, 5 -> 1.000000, 6 -> 1.000000), x + y)
    assertEquals(SparseVector(0 -> 2.000000, 1 -> 4.000000, 3 -> 6.000000), x + x)
    assertEquals(SparseVector(0 -> 2.000000, 1 -> 2.000000, 3 -> 3.000000, 4 -> 1.000000, 8 -> 1.000000), x + z)
    assertEquals(SparseVector(0 -> 1.000000, 4 -> 2.000000, 5 -> 1.000000, 6 -> 1.000000, 8 -> 1.000000), y + z)
    assertEquals(SparseVector(4 -> 2.000000, 5 -> 2.000000, 6 -> 2.000000), y + y)
    assertEquals(SparseVector(0 -> 2.000000, 4 -> 2.000000, 8 -> 2.000000), z + z)
  }
}