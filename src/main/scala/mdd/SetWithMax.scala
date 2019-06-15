package mdd

import java.util

import scala.collection.mutable

final class SetWithMax(length: Int) extends mutable.AbstractSet[Int]
  //  with mutable.SetOps[Int, mutable.Set, SetWithMax]
{

  private val candidates: util.BitSet = new util.BitSet(length)
  var max: Int = _

  clear()

  // @inline def +=(i: Int): SetWithMax.this.type = addOne(i)

  def addOne(i: Int): SetWithMax.this.type = throw new UnsupportedOperationException

  override def clear(): Unit = {
    max = length - 1
    candidates.set(0, length)
  }

  def clearFrom(newMax: Int): Unit = {
    if (newMax <= max) {
      max = candidates.previousSetBit(newMax - 1)
    }
    //println(s"clearFrom($newMax) -> $this")
  }

  override def foreach[U](f: Int => U): Unit = {
    var i = max
    while (i >= 0) {
      f(i)
      i = candidates.previousSetBit(i - 1)
    }
  }

  override def filter(f: Int => Boolean): SetWithMax = {
    var i = max
    while (i >= 0) {
      if (!f(i)) {
        this -= i
      }
      i = candidates.previousSetBit(i - 1)
    }
    this
  }

  // @inline def -=(i: Int): SetWithMax.this.type = subtractOne(i)

  def subtractOne(i: Int): SetWithMax.this.type = {
    candidates.clear(i)

    if (i == max) {
      max = candidates.previousSetBit(max - 1)
    }
    //println(s"- $i -> $this")
    this
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    var i: Int = SetWithMax.this.max

    def hasNext: Boolean = i >= 0

    def next(): Int = {
      val c = i
      i = candidates.previousSetBit(i - 1)
      c
    }
  }

  def contains(elem: Int): Boolean = elem <= max && candidates.get(elem)

}
