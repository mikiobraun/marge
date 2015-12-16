package marge.map

import collection.mutable.HashMap
import java.io.{ObjectInputStream, ObjectOutputStream}

/**
 * Twimpact
 *
 * User: mikio
 * Date: 4/7/11
 * Time: 3:24 PM
 */

/**
 * An NGram extractor, maps strings to maps
 */

class KeyDictionary[S] {
  private val keys = new HashMap[S, Int]
  private var last = 0

  def getKey(s: S): Int = keys.getOrElse(s, {
    val newKey = last
    last += 1
    keys(s) = newKey
    newKey
  })

  def save(out: ObjectOutputStream) {
    out.writeInt(keys.size)
    keys.foreach {kv => out.writeObject(kv._1); out.writeInt(kv._2)}
    out.writeInt(last)
  }

  def load(in: ObjectInputStream) {
    val count = in.readInt()
    for (i <- 1 to count) {
      val k: S = in.readObject().asInstanceOf[S]
      val v = in.readInt()
      keys(k) = v
    }
    last = in.readInt()
  }
}

object KeyDictionary {
  def load[S](in: ObjectInputStream): KeyDictionary[S] = {
    val kd = new KeyDictionary[S]
    kd.load(in)
    kd
  }
}
