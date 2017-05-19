package mdd

import java.util.IdentityHashMap

class IdMap[A, B] {
  private val idMap = new IdentityHashMap[A, B]

  def getOrElseUpdate(k: A, els: => B): B = {
    val existing = idMap.get(k)
    if (existing == null) {
      val update = els
      put(k, update)
      update
    } else {
      existing
    }
  }

  def put(k: A, v: B): Unit = {
    idMap.put(k, v)
  }

  def contains(k: A): Boolean = idMap.containsKey(k)

  def apply(k: A): B = idMap.get(k)

  def size: Int = idMap.size

}
