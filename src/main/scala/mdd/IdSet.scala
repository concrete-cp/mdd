package mdd

import java.util.IdentityHashMap
import scala.collection.JavaConverters._

final class IdSet[A] extends scala.collection.mutable.Set[A] {
  val idMap = new IdentityHashMap[A, Unit]
  def iterator: Iterator[A] = idMap.keySet.asScala.iterator

  def -=(elem: A) = {
    idMap.remove(elem)
    this
  }

  def +=(elem: A) = {
    idMap.put(elem, Unit)
    this
  }
  def contains(elem: A): Boolean = idMap.containsKey(elem)
}