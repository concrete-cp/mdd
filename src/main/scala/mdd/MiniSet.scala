package mdd

/**
  * Created by vion on 09/05/17.
  */
trait MiniSet {
  def present(i: Int): Boolean

  def size: Int

  def head: Int
}

class MySet(set: Set[Int]) extends MiniSet {
  def this(col: Int*) = this(col.toSet)

  def present(i: Int) = set(i)

  def size = set.size

  def head = set.min

  def filter(f: Int => Boolean): MySet = new MySet(set.filter(f))
}