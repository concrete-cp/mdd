package mdd

import java.util

class BIdMap[A1 <: AnyRef, A2 <: AnyRef, B] {
  private val map = new util.HashMap[IdTuple[A1, A2], B]

  def getOrElseUpdate(k1: A1, k2: A2, els: => B): B = {
    val tuple = new IdTuple(k1, k2)
    val existing = map.get(tuple)
    if (existing == null) {
      val update = els
      put(tuple, update)
      update
    } else {
      existing
    }
  }

  def put(t: IdTuple[A1, A2], v: B): Unit = {
    map.put(t, v)
  }

  def size: Int = map.size
}

class IdTuple[A <: AnyRef, B <: AnyRef](val a: A, val b: B) {
  override def equals(o: Any) = {
    o match {
      case t: IdTuple[_, _] => (a eq t.a) && (b eq t.b)
      case _ => false
    }
  }

  override def hashCode() = {
    System.identityHashCode(a) ^ System.identityHashCode(b)
  }
}
