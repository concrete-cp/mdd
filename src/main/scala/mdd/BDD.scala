package mdd

import java.util

import scala.collection.mutable

object BDD {
  def apply(mdd: MDD): BDD = {

    val map: IdMap[MDD, BDD] = new IdMap()

    def mdd2bdd(n: MDD): BDD = {
      if (n eq MDDLeaf) BDDLeaf
      else {
        map.getOrElseUpdate(n, {
          assert(n.children.sliding(2).forall {
            case Seq(_) => true
            case Seq(m1, m2) => m1._1 < m2._1
          })
          n.children.foldRight[BDD](BDD0) {
            case ((i, m), acc) => new BDDNode(i, mdd2bdd(m), acc)
          }
        })
      }
    }

    mdd2bdd(mdd)
  }
}

sealed trait BDD extends Iterable[Seq[Int]] with TSCached[BDD] {
  var id: Int = -1

  def +(e: List[Int]): BDD

  def index: Int

  def child: BDD

  def sibling: BDD

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
    val cache = new mutable.HashMap[(Int, Int, Int), Int]()
    val traversed = new TSSet[BDD]()


    def traverse(n: BDD, i: Int): Int = {
      n match {
        case n if (n eq BDD0) || (n eq BDDLeaf) => i
        case nt: BDDNode => traversed.onceOrElse(nt, {
          val is = traverse(nt.sibling, traverse(nt.child, i))

          val idc = nt.child.id
          val ids = nt.sibling.id

          val nid = cache.getOrElseUpdate((nt.index, idc, ids), is + 1)
          n.id = nid
          math.max(is, nid)
        }, i)
      }
    }

    traverse(this, 2)

  }

  def fastIdentify(): Int = {
    val traversed = new TSSet[BDD]()

    def traverse(n: BDD, i: Int): Int = {
      n match {
        case n if (n eq BDD0) || (n eq BDDLeaf) => i
        case nt: BDDNode => traversed.onceOrElse(nt, {
          val is = traverse(nt.sibling, traverse(nt.child, i)) + 1
          n.id = is
          is
        }, i)
      }
    }

    traverse(this, 2)

  }

  def filterTrie(doms: Array[Set[Int]], modified: List[Int], depth: Int = 0, ts: TSMap[BDD, BDD] = new TSMap[BDD, BDD]()): BDD

  def contains(e: Seq[Int]): Boolean

  def vertices(map: IdSet[BDD] = new IdSet()): Int

  final def edges(map: TSSet[BDD] = new TSSet(), total: Int = 0): Int = {
    if ((this eq BDD0) || (this eq BDDLeaf) || map.contains(this)) {
      total
    } else {
      map.put(this)
      // println(map.size + " " + total)
      sibling.edges(map, child.edges(map, total + 2))
    }
  }

  def depth(map: IdMap[BDD, Int] = new IdMap()): Int

  def supported(domains: Array[Set[Int]]): Array[util.HashSet[Int]] = {
    val arity = domains.length
    val newDomains = Array.fill(arity)(new util.HashSet[Int]())
    val sizes = Array.fill(arity)(0)

    fillFound(domains, newDomains, sizes, new SetWithMax(arity), 0, new TSSet())

    newDomains
  }

  def findSupport(ts: IdSet[BDD], scope: IndexedSeq[Set[Int]], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]] = {
    ???
  }

  def lambda(map: IdMap[BDD, BigInt] = new IdMap): BigInt

  protected[mdd] def fillFound(doms: Array[Set[Int]], newDomains: Array[util.HashSet[Int]], sizes: Array[Int],
                               l: SetWithMax, depth: Int, cache: TSSet[BDD]): Unit

  protected[mdd] def passTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD

  protected[mdd] def filterModifiedTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD
}

class BDDNode(val index: Int, val child: BDD, val sibling: BDD) extends BDD {
  assert(child.nonEmpty)

  def iterator: Iterator[Seq[Int]] = child.iterator.map(index +: _) ++ sibling.iterator

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

  def lambda(map: IdMap[BDD, BigInt]): BigInt =
    map.getOrElseUpdate(this, child.lambda(map) + sibling.lambda(map))

  def contains(e: Seq[Int]): Boolean = {
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

  def depth(map: IdMap[BDD, Int]): Int = {
    map.getOrElseUpdate(this, 1 + math.max(child.depth(map), sibling.depth(map)))
  }

  def vertices(map: IdSet[BDD]): Int = {
    if (map.contains(this)) {
      0
    } else {
      map.put(this)
      1 + child.vertices(map) + sibling.vertices(map)
    }
  }

  def filterTrie(doms: Array[Set[Int]], modified: List[Int], depth: Int, ts: TSMap[BDD, BDD]): BDD = {
    if (modified.isEmpty) {
      this
    } else if (modified.head == depth) {
      filterModifiedTrie(ts, doms, modified.tail, depth)
    } else {
      passTrie(ts, doms, modified, depth)
    }

  }

  def passTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD = {
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

  def filterModifiedTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD = {
    ts.getOrElseUpdate(this, {
      val newSibling = sibling.filterModifiedTrie(ts, doms, modified, depth)
      if (doms(depth).contains(index)) {
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

  protected[mdd] def fillFound(doms: Array[Set[Int]], newDomains: Array[util.HashSet[Int]], sizes: Array[Int],
                               l: SetWithMax, depth: Int, ts: TSSet[BDD]): Unit = {
    ts.once(
      this,
      if (depth <= l.max) {
        val od = newDomains(depth)

        if (od.add(index)) {
          sizes(depth) += 1
        }
        if (sizes(depth) == doms(depth).size) {
          l -= depth
        }
        child.fillFound(doms, newDomains, sizes, l, depth + 1, ts)
        sibling.fillFound(doms, newDomains, sizes, l, depth, ts)
      }
    )
  }

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

  def filterTrie(doms: Array[Set[Int]], modified: List[Int], depth: Int, ts: TSMap[BDD, BDD]): BDD =
    this

  def passTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD =
    this

  def filterModifiedTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD =
    this

  def identify(i: Int): Int = id

  def depth(map: IdMap[BDD, Int]) = 0

  def vertices(map: IdSet[BDD]): Int =
    if (map.contains(this)) 0
    else {
      map.put(this)
      1
    }

  def index = throw new UnsupportedOperationException("index of empty BDD")

  def child = throw new UnsupportedOperationException("child of empty BDD")

  def sibling = throw new UnsupportedOperationException("sibling of empty BDD")

  protected[mdd] def fillFound(doms: Array[Set[Int]], newDomains: Array[util.HashSet[Int]], sizes: Array[Int],
                               l: SetWithMax, depth: Int, cache: TSSet[BDD]): Unit = ()
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

  def filterTrie(doms: Array[Set[Int]], modified: List[Int], depth: Int, ts: TSMap[BDD, BDD]): BDD = {
    this
  }

  def passTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD =
    this

  def filterModifiedTrie(ts: TSMap[BDD, BDD], doms: Array[Set[Int]], modified: List[Int], depth: Int): BDD =
    this

  override def isEmpty = false

  def depth(map: IdMap[BDD, Int]) = 1

  def vertices(map: IdSet[BDD]): Int =
    if (map.contains(this)) 0
    else {
      map.put(this)
      1
    }

  def index = throw new UnsupportedOperationException("index of BDD leaf")

  def child = throw new UnsupportedOperationException("child of BDD leaf")

  def sibling = throw new UnsupportedOperationException("sibling of BDD leaf")

  protected[mdd] def fillFound(doms: Array[Set[Int]], newDomains: Array[util.HashSet[Int]], sizes: Array[Int],
                               l: SetWithMax, depth: Int, cache: TSSet[BDD]): Unit = {
    l.clearFrom(depth)
  }
}