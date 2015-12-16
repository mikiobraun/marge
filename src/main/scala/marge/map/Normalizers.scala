package marge.map

import marge.data.SparseVector

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/28/11
 * Time: 10:36 AM
 */

object Normalizers {
  def normalizeSum(vec: SparseVector): SparseVector = vec / vec.sum
}
