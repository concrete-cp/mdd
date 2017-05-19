package mdd

/**
  * Created by vion on 09/05/17.
  */
trait MiniSet extends Any {
  def present(i: Int): Boolean

  def size: Int

  def head: Int
}

object MySet {
  def apply(col:Int*) = new MySet(col.toSet)

  /**
    * Actually, just domains.map(_.head).min, just faster...
    * @param domains
    * @return
    */
  def compOffset(domains: Array[MiniSet]): Int = {
    var i = domains.length - 1
    var offset = Int.MaxValue
    while (i >= 0) {
      offset = math.min(offset, domains(i).head)
      i -= 1
    }
    offset
  }
}

class MySet(val set: Set[Int]) extends AnyVal with MiniSet {
  def present(i: Int) = set(i)

  def size = set.size

  def head = set.min

  def filter(f: Int => Boolean): MySet = new MySet(set.filter(f))
}