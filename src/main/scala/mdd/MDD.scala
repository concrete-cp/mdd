package mdd

import bitvectors.BitVector

import scala.collection.mutable.HashMap
import scala.util.hashing.MurmurHash3


object MDD {
  def apply(data: Traversable[Seq[Int]]): MDD = {
    data.foldLeft[MDD](MDD0)(
      (acc, tuple) => acc + tuple.toArray) //.reduce(new IdMap[Seq[MDD], MDD]())
  }

  def newTrie(i: Int, v: MDD) = {
    val t = new Array[MDD](i + 1) //Array.fill[MDD](i + 1)(MDD0)
    t(i) = v
    t
  }

  def apply(t: Map[Int, MDD]): MDD = {
    t.toSeq match {
      case Seq() => MDD0
      case Seq((index, child)) => new MDD1(child, index)
      case Seq((i1, c1), (i2, c2)) => new MDD2(c1, i1, c2, i2)
      case e: Any => new MDDn(newTrie(e: _*))
    }
  }

  //
  def newTrie(t: (Int, MDD)*) = {
    val s: Int = t.map(_._1).max
    val trie = new Array[MDD](s + 1)
    for ((i, v) <- t) {
      trie(i) = v
    }
    trie
  }

  def same(t1: Map[Int, MDD], t2: Map[Int, MDD]): Boolean = {
    //t1.hashCode == t2.hashCode &&
    t1.size == t2.size &&
      t1.forall {
        case (k1, v1) => t2.get(k1).exists(v1 eq _)
      }
  }

}

trait MDD extends Iterable[Seq[Int]] {

  override lazy val hashCode: Int = {
    //logger.warn("Computed hashcode")
    MurmurHash3.unorderedHash(traverseST)
  }
  val cache = new TSCache[MDD]()
  private var _id: Int = _

  def id = _id

  def identify(ts: Int, i: Int = 1): Int = {
    if (this eq MDDLeaf) {
      i
    } else {
      cache(ts, i, {
        var id = i
        _id = id
        forSubtries {
          (_, t) =>
            id = t.identify(ts, id + 1)
            true
        }
        id
      })
    }
  }

  def +(t: Seq[Int]): MDD = {
    val ta = t.toArray
    if (contains(ta)) MDD.this else addTrie(ta, 0)
  }

  //  def reduce(mdds: collection.mutable.Map[Map[Int, MDD], MDD] = new HashMap()): MDD = {
  //    MDD.nodes += 1
  //    val current = traverseST.toMap
  //
  //    mdds.getOrElseUpdate(current, {
  //
  //      val reduced = current.map { case (i, j) => i -> j.reduce(mdds) }
  //
  //      if (same(current, reduced)) {
  //        this
  //      } else {
  //        mdds.getOrElseUpdate(reduced, MDD(reduced))
  //      }
  //
  //    })
  //  }

  final def contains(t: Array[Int]): Boolean = contains(t, 0)

  def addTrie(t: Array[Int], i: Int): MDD

  def reduce(): MDD = {

    val cache = new HashMap[Map[Int, Int], MDD]()
    val id = new IdMap[MDD, Int]()

    id(MDD0) = 0
    id(MDDLeaf) = 1
    var i = 2

    def step1(n: MDD): Unit = n match {
      case MDD0 | MDDLeaf => ()
      case n if !id.contains(n) =>
        for ((_, c) <- n.traverseST) step1(c)

        val idc = n.traverseST
          .map { case (i, c) => i -> id(c) }
          .toMap

        cache.get(idc) match {
          case Some(m) => id(n) = id(m)
          case None => id(n) = i; i += 1
        }

        cache.update(idc, n)
      case _ => ()
    }

    step1(this)

    val common = new Array[MDD](i + 1)
    common(0) = MDD0
    common(1) = MDDLeaf

    def step2(n: MDD): MDD = {

      val idn = id(n)
      if (common(idn) == null) {
        common(idn) = MDD(n.traverseST.toMap.mapValues(step2)) //new MDDLinkNode(nt.index, step2(nt.child), step2(nt.sibling))
      }
      common(idn)

    }

    step2(this)

  }

  def contains(t: Array[Int], i: Int): Boolean

  def findSupport(ts: Int, scope: IndexedSeq[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]]

  def checkSup(ts: Int, domains: IndexedSeq[MiniSet], p: Int, i: Int, index: Int, next: MDD, support: Array[Int], depth: Int) = {
    val ok = if (p == depth) {
      i == index
    } else domains(depth).present(index)
    if (ok) {
      support(depth) = index
      next.findSupport(ts, domains, p, i, support, depth + 1)
    } else {
      None
    }
  }

  def edges(ts: Int): Int

  def vertices(map: IdMap[MDD, Unit]): Int = {
    if (map.contains(this)) {
      0
    } else if (this eq MDDLeaf) {
      map.put(this, ())
      1
    } else {
      map.put(this, ())
      1 + traverseST.map { case (_, m) => m.vertices(map) }.sum
    }
  }

  def filterTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int): MDD

  def supported(ts: Int, doms: Array[MiniSet], newDomains: Array[BitVector], depth: Int, l: SetWithMax): Unit

  final def lambda: BigInt = {
    lambda(new IdMap())
  }

  final def lambda(map: IdMap[MDD, BigInt]): BigInt = {
    if (this eq MDDLeaf) {
      BigInt(1)
    } else {
      map.getOrElseUpdate(this, traverseST.map { case (_, m) => m.lambda(map) }.sum)
    }
  }

  def depth(map: IdMap[MDD, Int]): Int = {
    if (this eq MDD0) 0
    else if (this eq MDDLeaf) 1
    else map.getOrElseUpdate(this, 1 + traverseST.map { case (_, m) => m.depth(map) }.max)
  }

  def forSubtries(f: (Int, MDD) => Boolean): Boolean

  override def isEmpty: Boolean

  def copy(ts: Int): MDD

  override def equals(o: Any): Boolean = o match {
    case MDDLeaf => this eq MDDLeaf
    case t: MDD => {
      val t1 = t.traverseST.toMap
      val t2 = traverseST.toMap

      MDD.same(t1, t2)
    }
    //
    //      t.traverseST.toIterable.zip(traverseST.toIterable).forall {
    //        case ((i1, s1), (i2, s2)) => i1 == i2 && (s1 eq s2)
    //      }
    case _ => false
  }

  def traverseST: Traversable[(Int, MDD)] = new Traversable[(Int, MDD)] {
    def foreach[A](f: ((Int, MDD)) => A) {
      forSubtries { (i, mdd) => f((i, mdd)); true }
    }
  }

  def subMDD(i: Int): MDD

  override def toString = System.identityHashCode(this).toString

  def nodes(map: IdMap[MDD, Unit]): IdMap[MDD, Unit] = {
    if (map.contains(this)) {
      map
    } else {
      traverseST.foldLeft[IdMap[MDD, Unit]](map += ((this, ()))) {
        case (map, (_, MDDLeaf)) => map
        case (map, (_, n)) => n.nodes(map)
      }
    }
  }
}

object MDDLeaf extends MDD {
  override lazy val hashCode = 0

  //override def size = 1

  override def id = 0

  //override def reduce(mdds: collection.mutable.Map[Map[Int, MDD], MDD]) = this
  def contains(tuple: Array[Int], i: Int) = true

  def iterator = Iterator(Nil)

  def edges(ts: Int) = 0

  def filterTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int) = {
    assert(modified.isEmpty)
    this
  }

  def supported(ts: Int, doms: Array[MiniSet], newDoms: Array[BitVector], depth: Int, l: SetWithMax) = {
    //assert(depth > l.max)
    //println("leaf at depth " + depth)
    l.clearFrom(depth)
  }

  def addTrie(tuple: Array[Int], i: Int) = {
    require(i >= tuple.length)
    this //throw new UnsupportedOperationException
  }

  override def toString = "MDD Leaf"

  def forSubtries(f: (Int, MDD) => Boolean): Boolean = {
    throw new UnsupportedOperationException
  }

  override def size = 1

  override def isEmpty = false

  def findSupport(ts: Int, scope: IndexedSeq[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int) =
    Some(support)

  override def equals(o: Any) = o match {
    case r: AnyRef => r eq MDDLeaf
    case _ => false
  }

  def copy(ts: Int) = this

  def subMDD(i: Int) = MDDLeaf

}

final object MDD0 extends MDD {
  override def id = throw new UnsupportedOperationException

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = MDD0

  def contains(tuple: Array[Int], i: Int) = false

  def iterator = Iterator()

  def edges(ts: Int) = 0

  def filterTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int) = MDD0

  def supported(ts: Int, doms: Array[MiniSet], nd: Array[BitVector], depth: Int, l: SetWithMax) = {
    throw new UnsupportedOperationException
  }

  def addTrie(tuple: Array[Int], i: Int) = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      new MDD1(MDD0.addTrie(tuple, i + 1), tuple(i))
    }
  }

  override def toString = "Empty MDD"

  def forSubtries(f: (Int, MDD) => Boolean) = true

  override def isEmpty = true

  def findSupport(ts: Int, scope: IndexedSeq[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int) =
    None

  def copy(ts: Int) = this

  def subMDD(i: Int) = MDD0
}

final class MDD1(private val child: MDD, private val index: Int) extends MDD {
  assert(child ne MDD0)

  def forSubtries(f: (Int, MDD) => Boolean) = {
    f(index, child)
  }

  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      val v = t(i)
      if (v == index) {

        val nc = child.addTrie(t, i + 1)
        if (nc eq child) {
          this
        } else {
          new MDD1(nc, v)
        }

      } else {
        new MDD2(child, index, MDD0.addTrie(t, i + 1), v)
      }
    }

  def contains(t: Array[Int], i: Int): Boolean = {
    t(i) == index && child.contains(t, i + 1)
  }

  //  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
  //    val b = child.reduce(mdds)
  //    val newArray = MDD.newTrie(index, b)
  //    mdds.getOrElseUpdate(newArray, new MDD1(b, index))
  //
  //  }

  def findSupport(ts: Int, scope: IndexedSeq[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int) = {
    cache(ts, None,
      checkSup(ts, scope, p, i, index, child, support, depth))
  }

  def supported(ts: Int, doms: Array[MiniSet], newDomains: Array[BitVector], depth: Int, l: SetWithMax): Unit = {
    cache(ts, (), {
      if (depth <= l.max) {
        newDomains(depth) += index
        if (newDomains(depth).cardinality == doms(depth).size) l -= depth
        child.supported(ts, doms, newDomains, depth + 1, l)
      }
    })
  }

  def filterTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else {
      cache(ts) {
        val nC =
          if (modified.head == depth) {
            // Some change at this level
            if (doms(depth).present(index)) {
              child.filterTrie(ts, doms, modified.tail, depth + 1)
            } else {
              MDD0
            }
          } else {
            // No change at this level (=> no need to call f())
            child.filterTrie(ts, doms, modified, depth + 1)
          }

        if (nC eq MDD0) {
          MDD0
        } else if (nC eq child) {
          this
        } else {
          new MDD1(nC, index)
        }
      }

    }

  def iterator = child.iterator.map(index +: _)

  def edges(ts: Int): Int = cache(ts, 0, 1 + child.edges(ts))

  override def isEmpty = false

  def copy(ts: Int) = cache(ts)(new MDD1(child.copy(ts), index))

  def subMDD(i: Int) = if (index == i) child else MDD0
}

final class MDD2(
                  private val left: MDD, private val leftI: Int,
                  private val right: MDD, private val rightI: Int) extends MDD {
  assert(right ne MDD0)
  assert(left ne MDD0)

  def forSubtries(f: (Int, MDD) => Boolean) = {
    f(leftI, left) && f(rightI, right)
  }

  def addTrie(t: Array[Int], i: Int): MDD =
    if (i >= t.length) {
      MDDLeaf
    } else {
      t(i) match {
        case `leftI` =>
          val nl = left.addTrie(t, i + 1)
          if (nl eq left) {
            this
          } else {
            new MDD2(nl, leftI, right, rightI)
          }
        case `rightI` =>
          val nr = right.addTrie(t, i + 1)
          if (nr eq right) {
            this
          } else {
            new MDD2(left, leftI, nr, rightI)
          }
        case v: Int =>
          val newArray = MDD.newTrie((leftI, left), (rightI, right), (v, MDD0.addTrie(t, i + 1)))
          new MDDn(newArray)

      }
    }

  def contains(t: Array[Int], i: Int): Boolean = t(i) match {
    case `leftI` => left.contains(t, i + 1)
    case `rightI` => right.contains(t, i + 1)
    case _ => false
  }

  def supported(ts: Int, doms: Array[MiniSet], newDoms: Array[BitVector], depth: Int, l: SetWithMax): Unit =
    cache(ts, (), {
      if (depth <= l.max) {
        newDoms(depth) += leftI
        newDoms(depth) += rightI
        if (newDoms(depth).cardinality == doms(depth).size) l -= depth
        left.supported(ts, doms, newDoms, depth + 1, l)
        right.supported(ts, doms, newDoms, depth + 1, l)
      }
    })

  def filterTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int): MDD =
    if (modified.isEmpty) {
      this
    } else cache(ts) {
      var nL: MDD = null
      var nR: MDD = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(ts, doms, modified.tail, depth, left, leftI)
        nR = filteredTrie(ts, doms, modified.tail, depth, right, rightI)
      } else {
        // No change at this level (=> no need to check presence)
        nL = left.filterTrie(ts, doms, modified, depth + 1)
        nR = right.filterTrie(ts, doms, modified, depth + 1)
      }

      if (nL eq MDD0) {
        if (nR eq MDD0) {
          MDD0
        } else {
          new MDD1(nR, rightI)
        }
      } else if (nR eq MDD0) {
        new MDD1(nL, leftI)
      } else {
        if ((nL eq left) && (nR eq right)) {
          this
        } else {
          new MDD2(nL, leftI, nR, rightI)
        }
      }

    }

  @inline
  private def filteredTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int, t: MDD, i: Int) = {
    if (doms(depth).present(i)) {
      t.filterTrie(ts, doms, modified, depth + 1)
    } else {
      MDD0
    }
  }

  def iterator: Iterator[Seq[Int]] =
    left.iterator.map(leftI +: _) ++ right.iterator.map(rightI +: _)

  def edges(ts: Int): Int = cache(ts, 0, 2 + left.edges(ts) + right.edges(ts))

  //  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]): MDD = {
  //    val bL = left.reduce(mdds)
  //    val bR = right.reduce(mdds)
  //
  //    val nT = MDD.newTrie((leftI, bL), (rightI, bR))
  //
  //    mdds.getOrElseUpdate(nT, new MDD2(bL, leftI, bR, rightI))
  //  }

  override def isEmpty = false

  def findSupport(ts: Int, scope: IndexedSeq[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int) =
    cache(ts, None,
      checkSup(ts, scope, p, i, leftI, left, support, depth).orElse(
        checkSup(ts, scope, p, i, rightI, right, support, depth)))

  def copy(ts: Int) = cache(ts)(new MDD2(left.copy(ts), leftI, right.copy(ts), rightI))

  def subMDD(i: Int) = i match {
    case `leftI` => left
    case `rightI` => right
    case _ => MDD0
  }
}

final class MDDn(
                  private val trie: Array[MDD]) extends MDD {

  assert(indicesIterator.nonEmpty)
  assert(trie.forall(m => (m eq null) || m.nonEmpty))

  def copy(ts: Int) = cache(ts)(new MDDn(trie.map(t => if (t eq null) null else t.copy(ts))))

  def forSubtries(f: (Int, MDD) => Boolean) = {
    var i = 0
    var continue = true
    while (i < trie.length && continue) {
      if (!isEmpty(trie(i))) {
        continue = f(i, trie(i))
      }
      i += 1
    }
    continue
  }

  def addTrie(tuple: Array[Int], i: Int): MDD = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newTrie = trie.padTo(v + 1, null)

      if (isEmpty(newTrie(v))) {
        newTrie(v) = MDD0.addTrie(tuple, i + 1)
        new MDDn(newTrie)
      } else {
        val ntv = newTrie(v).addTrie(tuple, i + 1)

        if (ntv eq newTrie(v)) {
          this
        } else {
          newTrie(v) = ntv
          new MDDn(newTrie)
        }
      }

    }
  }

  def contains(tuple: Array[Int], i: Int): Boolean = {
    val v = tuple(i)
    v < trie.length && !isEmpty(trie(v)) && trie(v).contains(tuple, i + 1)
  }

  def supported(ts: Int, doms: Array[MiniSet], newDomains: Array[BitVector], depth: Int, l: SetWithMax): Unit = cache(ts, (), {

    forIndices { i =>
      newDomains(depth) += i
    }

    if (newDomains(depth).cardinality == doms(depth).size) l -= depth

    if (depth <= l.max) {
      forIndices { i =>
        trie(i).supported(ts, doms, newDomains, depth + 1, l)
      }
    }
  })

  private def forIndices[U](f: Int => U): Unit = {
    for (i <- 0 until trie.length) {
      if (!isEmpty(trie(i))) {
        f(i)
      }
    }
  }

  private def isEmpty(t: MDD) = t eq null

  def filterTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int = 0): MDD =
    if (modified.isEmpty) {
      this
    } else cache(ts) {

      val newTrie =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(ts, doms, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(ts, doms, modified, depth + 1)
        }

      if (same(newTrie, trie)) {
        this
      } else {
        newNode(newTrie)
      }

    }

  def iterator = indicesIterator.map(i => (trie(i), i)) flatMap {
    case (t, i) => t.iterator map (i +: _)
  }

  def indicesIterator = new Traversable[Int] {
    def foreach[U](f: Int => U): Unit = forIndices(f)
  }
    .toIterator

  def edges(ts: Int): Int = cache(ts, 0, {
    var e = 0
    forIndices { i =>
      e += 1 + trie(i).edges(ts)
    }
    e
  })

  override def isEmpty = false

  def findSupport(ts: Int, scope: IndexedSeq[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int): Option[Array[Int]] =
    cache(ts, None, {

      if (depth == p) {
        if (i >= trie.length || isEmpty(trie(i))) {
          None
        } else {
          support(depth) = i
          trie(i).findSupport(ts, scope, p, i, support, depth + 1)
        }
      } else {

        forIndices { index =>
          if (scope(depth).present(index)) {
            support(depth) = index
            val s = trie(index).findSupport(ts, scope, p, i, support, depth + 1)
            if (s.nonEmpty) return s
          }
        }
        None
      }
    })

  def subMDD(i: Int) = {
    if (i >= trie.length || isEmpty(trie(i))) {
      MDD0
    } else {
      trie(i)
    }
  }

  private def filteredTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int): Array[MDD] = {

    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)

    forIndices { i =>
      doms(depth).present(i) && {
        val uT = trie(i).filterTrie(ts, doms, modified, depth + 1)
        if (uT eq MDD0) {
          false
        } else {
          newTrie(i) = uT
          true
        }
      }
    }

    newTrie
  }

  private def passedTrie(ts: Int, doms: Array[MiniSet], modified: List[Int], depth: Int): Array[MDD] = {
    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)

    forIndices { i =>
      val nT = trie(i).filterTrie(ts, doms, modified, depth)
      if (nT eq MDD0) {
        false
      } else {
        newTrie(i) = nT
        true
      }
    }
    newTrie
  }

  private def newNode(t: Array[MDD]): MDD = {
    var i = t.length - 1
    while (i >= 0 && isEmpty(t(i))) {
      i -= 1
    }
    if (i < 0) {
      MDD0
    } else {
      val i1 = i
      while (i >= 0 && isEmpty(t(i))) {
        i -= 1
      }
      if (i < 0) {
        new MDD1(t(i1), i1)
      } else {
        val i2 = i
        while (i >= 0 && isEmpty(t(i))) {
          i -= 1
        }
        if (i < 0) {
          new MDD2(t(i2), i2, t(i1), i1)
        } else {
          new MDDn(t)
        }
      }
    }
  }

  private def same(t1: Array[MDD], t2: Array[MDD]): Boolean = {
    def empty(t: Array[MDD], i: Int) = {
      i >= t.length || isEmpty(t(i))
    }

    (0 until math.max(t1.length, t2.length)).forall { i =>
      (empty(t1, i) && empty(t2, i)) || (t1(i) eq t2(i))
    }
  }
}


