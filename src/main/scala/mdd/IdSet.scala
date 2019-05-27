package mdd

import java.util

import scala.collection.JavaConverters._

final class IdSet[A] {
  private val idMap = new util.IdentityHashMap[A, Unit]

  def iterator: Iterator[A] = idMap.keySet.asScala.iterator

  def onceOrElse[B](elem: A, op: => B, els: => B): B = {
    if (contains(elem)) {
      els
    } else {
      this.put(elem)
      op
    }
  }

  def put(elem: A): Unit = {
    idMap.put(elem, ())
  }

  def contains(elem: A): Boolean = idMap.containsKey(elem)

  def once[U](elem: A, op: => U): Unit = {
    if (!contains(elem)) {
      this.put(elem)
      op
    }
  }

  def size: Int = idMap.size
}