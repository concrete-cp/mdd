package mdd

import java.util.IdentityHashMap
import scala.collection.JavaConverters._

final class IdMap[A, B] extends scala.collection.mutable.Map[A, B] {
  val idMap = new IdentityHashMap[A, B]

  def +=(kv: (A, B)) = {
    idMap.put(kv._1, kv._2)
    this
  }

  def -=(key: A) = {
    idMap.remove(key)
    this
  }

  def get(key: A) = Option(idMap.get(key))

  def iterator = idMap.asScala.iterator

  override def size = idMap.size

}