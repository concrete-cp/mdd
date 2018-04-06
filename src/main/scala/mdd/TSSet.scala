package mdd

object TSSet {
  var ts = 0

  def nextTS(): Int = {
    val t = ts
    ts += 1
    t
  }
}

/**
  * Created by vion on 06/06/17.
  */
class TSSet[A <: Timestampped[_]](val timestamp: Int = TSSet.nextTS()) {
  def onceOrElse[B](elem: A, op: => B, els: => B): B = {
    if (contains(elem)) {
      els
    } else {
      this.put(elem)
      op
    }
  }

  def put(elem: A): Unit = {
    elem.timestamp = timestamp
  }

  def contains(elem: A): Boolean = elem.timestamp == timestamp

  def once[U](elem: A, op: => U): Unit = {
    if (!contains(elem)) {
      this.put(elem)
      op
    }
  }
}

trait Timestampped[A] {
  var timestamp: Int = -1
}

trait TSCached[A] extends Timestampped[A] {
  var previous: A = _
}

class TSMap[A <: TSCached[B], B](val timestamp: Int = TSSet.nextTS()) {
  def getOrElseUpdate(k: A, els: => B): B = {
    if (contains(k)) {
      k.previous
    } else {
      val update = els
      put(k, update)
      update
    }
  }

  def put(k: A, v: B): Unit = {
    k.timestamp = timestamp
    k.previous = v
  }

  def contains(k: A): Boolean = k.timestamp == timestamp

  def get(k: A): Option[B] = if (contains(k)) Some(k.previous) else None
}