package mdd

import bitvectors.BitVector

import scala.collection.mutable.HashMap

object BDD {
  def apply(mdd: MDD): BDD = {

    val map: IdMap[MDD, BDD] = new IdMap()

    def mdd2bdd(n: MDD): BDD = {
      if (n eq MDDLeaf) BDDLeaf
      else {
        map.getOrElseUpdate(n, {
          n.traverseST.toSeq.sortBy(-_._1).foldLeft[BDD](BDD0) {
            case (acc, (i, m)) => new BDDNode(i, mdd2bdd(m), acc)
          }
        })
      }
    }

    mdd2bdd(mdd)
  }
}

sealed trait BDD extends Iterable[Seq[Int]] {
  var id: Int = -1

  def +(e: List[Int]): BDD

  def reduce(): BDD = {

    val i = identify()

    val common = new Array[BDD](i + 1)
    common(0) = BDD0
    common(1) = BDDLeaf

    def step2(n: BDD): BDD = {

      val idn = n.id
      if (common(idn) == null) {
        val nt = n.asInstanceOf[BDDNode]
        common(idn) = new BDDNode(nt.index, step2(nt.child), step2(nt.sibling))
      }
      common(idn)

    }

    step2(this)

  }

  def identify(): Int = {
    val cache = new HashMap[(Int, Int, Int), BDD]()

    def traverse(n: BDD, i: Int): Int = {
      n match {
        case nt: BDDNode if n.id < 0 =>
          val is = traverse(nt.sibling, traverse(nt.child, i))

          val idc = nt.child.id
          val ids = nt.sibling.id

          cache.get((nt.index, idc, ids)) match {
            case Some(m) =>
              n.id = m.id
            case None =>
              n.id = is + 1
          }

          cache((nt.index, idc, ids)) = n
          math.max(is, n.id)
        case _ => math.max(n.id, i)
      }
    }

    traverse(this, 1)

  }

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int = 0, ts: IdMap[BDD, BDD] = new IdMap): BDD

  def contains(e: Seq[Int]): Boolean

  def vertices(map: IdSet[BDD] = new IdSet()): Int

  def edges(map: IdSet[BDD] = new IdSet()): Int

  def depth(map: IdMap[BDD, Int] = new IdMap()): Int

  def supported(domains: Array[MiniSet]): (Array[BitVector], Int) = {
    val arity = domains.length
    val newDomains = Array.fill[BitVector](arity)(BitVector.empty)
    val sizes = Array.fill(arity)(0)
    val offset = MySet.compOffset(domains)
    //
    //    def updNewDomains(depth: Int, i: Int) = {
    //      val od = newDomains(depth)
    //      val nd = od + (i - offset)
    //      if (od != nd) {
    //        newDomains(depth) = nd
    //        sizes(depth) += 1
    //      }
    //      sizes(depth) == domains(depth).size
    //    }

    fillFound(domains, newDomains, offset: Int, sizes, new SetWithMax(arity), 0, new IdSet)

    (newDomains, offset)
  }

  def findSupport(ts: IdSet[BDD], scope: IndexedSeq[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]] = {
    ???
  }

  def lambda(map: IdMap[BDD, BigInt] = new IdMap): BigInt

  protected[mdd] def fillFound(doms: Array[MiniSet], newDomains: Array[BitVector], offset: Int, sizes: Array[Int],
                               l: SetWithMax, depth: Int, cache: IdSet[BDD]): Unit

  protected[mdd] def passTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD

  protected[mdd] def filterModifiedTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD
}

object BDD0 extends BDD {
  id = 0

  def iterator = Iterator.empty

  def +(e: List[Int]): BDD = e match {
    case Nil => BDDLeaf
    case h :: t => new BDDNode(h, BDD0 + t, BDD0)
  }

  def lambda(map: IdMap[BDD, BigInt]) = 0

  def contains(e: Seq[Int]) = false

  def reduce(cache: collection.mutable.Map[BDD, BDD]) = BDD0

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[BDD, BDD]): BDD =
    this

  def passTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD =
    this

  def filterModifiedTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD =
    this

  def identify(i: Int) = id

  def depth(map: IdMap[BDD, Int]) = 0

  def vertices(map: IdSet[BDD]) =
    if (map.contains(this)) 0
    else {
      map.put(this)
      1
    }

  def edges(map: IdSet[BDD]) = 0

  protected[mdd] def fillFound(doms: Array[MiniSet], newDomains: Array[BitVector], offset: Int, sizes: Array[Int],
                               l: SetWithMax, depth: Int, cache: IdSet[BDD]): Unit = ()
}

object BDDLeaf extends BDD {
  id = 1

  def iterator = Iterator(Seq())

  def +(e: List[Int]): BDD = {
    require(e.isEmpty)
    this
  }

  def lambda(map: IdMap[BDD, BigInt]) = 1

  def contains(e: Seq[Int]) = true

  def reduce(cache: collection.mutable.Map[BDD, BDD]) = BDDLeaf

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[BDD, BDD]): BDD = {
    this
  }

  def passTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD =
    this

  def filterModifiedTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD =
    this

  override def isEmpty = false

  def depth(map: IdMap[BDD, Int]) = 1

  def vertices(map: IdSet[BDD]) =
    if (map.contains(this)) 0
    else {
      map.put(this)
      1
    }

  def edges(map: IdSet[BDD]) = 0

  protected[mdd] def fillFound(doms: Array[MiniSet], newDomains: Array[BitVector], offset: Int, sizes: Array[Int],
                               l: SetWithMax, depth: Int, cache: IdSet[BDD]): Unit = {
    l.clearFrom(depth)
  }
}

class BDDNode(val index: Int, val child: BDD, val sibling: BDD) extends BDD {
  assert(child.nonEmpty)

  def iterator = child.iterator.map(index +: _) ++ sibling.iterator

  def +(e: List[Int]): BDD = {
    val h :: t = e
    if (index > h) {
      new BDDNode(h, BDD0 + t, this)
    } else if (index == h) {
      new BDDNode(index, child + t, sibling)
    } else {
      new BDDNode(index, child, sibling + e)
    }
  }

  def lambda(map: IdMap[BDD, BigInt]) =
    map.getOrElseUpdate(this, child.lambda(map) + sibling.lambda(map))

  def contains(e: Seq[Int]) = {
    val h +: t = e
    if (h < index) {
      false
    } else if (index == h) {
      child.contains(t)
    } else {
      sibling.contains(e)
    }
  }

  override def hashCode = ???

  override def equals(o: Any) = o match {
    case n: BDDNode => (n.index == index) && (n.child eq child) && (n.sibling eq sibling)
    case _ => false
  }

  def depth(map: IdMap[BDD, Int]) = {
    map.getOrElseUpdate(this, 1 + math.max(child.depth(map), sibling.depth(map)))
  }

  def vertices(map: IdSet[BDD]) =
    map.onceOrElse(this, 1 + child.vertices(map) + sibling.vertices(map), 0)

  def edges(map: IdSet[BDD]) =
    map.onceOrElse(this,
      2 + child.edges(map) + sibling.edges(map)
      , 0)

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[BDD, BDD]): BDD = {
    if (modified.isEmpty) {
      this
    } else if (modified.head == depth) {
      filterModifiedTrie(ts, doms, modified.tail, depth)
    } else {
      passTrie(ts, doms, modified, depth)
    }

  }

  def passTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD = {
    ts.getOrElseUpdate(this, {
      val newChild = child.filterTrie(doms, modified, depth + 1, ts)
      val newSibling = sibling.passTrie(ts, doms, modified, depth)
      if (newChild.isEmpty) {
        newSibling
      } else if ((child eq newChild) && (sibling eq newSibling)) {
        this
      } else {
        new BDDNode(index, newChild, newSibling)
      }
    })
  }

  def filterModifiedTrie(ts: IdMap[BDD, BDD], doms: Array[MiniSet], modified: List[Int], depth: Int): BDD = {
    ts.getOrElseUpdate(this, {
      val newSibling = sibling.filterModifiedTrie(ts, doms, modified, depth)
      if (doms(depth).present(index)) {
        val newChild = child.filterTrie(doms, modified, depth + 1, ts)
        if (newChild.isEmpty) {
          newSibling
        } else if ((child eq newChild) && (sibling eq newSibling)) {
          this
        } else {
          new BDDNode(index, newChild, newSibling)
        }
      } else {
        newSibling
      }
    })
  }

  override def isEmpty = false

  protected[mdd] def fillFound(doms: Array[MiniSet], newDomains: Array[BitVector], offset: Int, sizes: Array[Int],
                               l: SetWithMax, depth: Int, ts: IdSet[BDD]): Unit = {
    ts.once(
      this,
      if (depth <= l.max) {
        val od = newDomains(depth)
        val nd = od + (index - offset)
        if (od != nd) {
          newDomains(depth) = nd
          sizes(depth) += 1
        }
        if (sizes(depth) == doms(depth).size) {
          l -= depth
        }
        child.fillFound(doms, newDomains, offset, sizes, l, depth + 1, ts)
        sibling.fillFound(doms, newDomains, offset, sizes, l, depth, ts)
      }
    )
  }

}