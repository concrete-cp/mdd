package mdd

import java.util

final class SetWithMax(length: Int) extends collection.mutable.Set[Int] with collection.mutable.SetLike[Int, SetWithMax] {
  var max: Int = length - 1
  private val candidates: util.BitSet = new util.BitSet(length)

  candidates.set(0, length) //BitVector.filled(length)

  override  def empty: SetWithMax = new SetWithMax(0)

  def +=(i: Int): SetWithMax.this.type = ???

  def -=(i: Int): SetWithMax.this.type = {
    candidates.clear(i)

    if (i == max) {
      max = candidates.previousSetBit(max - 1)
    }
    //println(s"- $i -> $this")
    this
  }

  def clearFrom(newMax: Int): Unit = {
    if (newMax <= max) {
      max = candidates.previousSetBit(newMax - 1)
    }
    //println(s"clearFrom($newMax) -> $this")
  }

  override def foreach[U](f: Int => U) {
    var i = max
    while (i >= 0) {
      f(i)
      i = candidates.previousSetBit(i-1)
    }
  }

  override def filter(f: Int => Boolean): SetWithMax = {
    var i = max
    while (i >= 0) {
      if (!f(i)) {
        this -= i
      }
      i = candidates.previousSetBit(i-1)
    }
    this
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    var i: Int = SetWithMax.this.max
    def hasNext: Boolean = i >= 0
    def next(): Int = {
      val c = i
      i = candidates.previousSetBit(i-1)
      c
    }
  }

  def contains(elem: Int): Boolean = elem <= max && candidates.get(elem)

}
