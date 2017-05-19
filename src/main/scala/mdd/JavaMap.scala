package mdd

import java.util.HashMap

import scala.collection.JavaConverters._
import scala.collection.mutable

class JavaMap[A, B] extends mutable.Map[A, B] {
  private val map = new HashMap[A, B]

  def +=(kv: (A, B)) = {
    map.put(kv._1, kv._2)
    this
  }

  def -=(key: A) = {
    map.remove(key)
    this
  }

  def get(key: A): Option[B] = Option(map.get(key))

  def iterator: Iterator[(A, B)] = map.asScala.iterator

  override def size: Int = map.size

}
