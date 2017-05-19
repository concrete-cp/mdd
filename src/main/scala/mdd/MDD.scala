package mdd

import bitvectors.BitVector

import scala.util.hashing.MurmurHash3


object MDD {
  def apply(data: Traversable[Seq[Int]]): MDD = {
    data.foldLeft[MDD](MDD0)(
      (acc, tuple) => acc + tuple.toArray) //.reduce(new IdMap[Seq[MDD], MDD]())
  }

  def newTrie(i: Int, v: MDD): Array[MDD] = {
    val t = new Array[MDD](i + 1) //Array.fill[MDD](i + 1)(MDD0)
    t(i) = v
    t
  }

  def apply(t: Seq[(Int, MDD)]): MDD = {
    assert(t.map(_._1).distinct.size == t.size)
    t match {
      case Seq() => MDD0
      case Seq((index, child)) => new MDD1(child, index)
      case Seq((i1, c1), (i2, c2)) => new MDD2(c1, i1, c2, i2)
      case e: Any => new MDDn(newTrie(e: _*))
    }
  }

  def newTrie(t: (Int, MDD)*): Array[MDD] = {
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

  private var _id: Int = _

  def id = _id

  def identify(cache: IdSet[MDD] = new IdSet(), i: Int = 1): Int = {
    if (this eq MDDLeaf) {
      i
    } else {
      cache.onceOrElse(this, {
        var id = i
        _id = id
        forSubtries { (_, t) =>
          id = t.identify(cache, id + 1)
          true
        }
        id
      }, i)
    }
  }

  def +(t: Seq[Int]): MDD = {
    val ta = t.toArray
    if (contains(ta)) MDD.this else addTrie(ta, 0)
  }

  def addTrie(t: Array[Int], i: Int): MDD

  def reduce(): MDD = {

    val cache = new java.util.HashMap[Map[Int, Int], MDD]()
    val id = new IdMap[MDD, Int]()

    id.put(MDD0, 0)
    id.put(MDDLeaf, 1)
    var i = 2

    def step1(n: MDD): Unit = {
      if (!id.contains(n)) {
        for ((_, c) <- n.traverseST) step1(c)

        val idc = n.traverseST
          .map { case (i, c) => i -> id(c) }
          .toMap

        cache.get(idc) match {
          case null => id.put(n, i); i += 1
          case m => id.put(n, id(m))
        }

        cache.put(idc, n)
      }
    }

    step1(this)

    val common = new Array[MDD](i + 1)
    common(0) = MDD0
    common(1) = MDDLeaf

    def step2(n: MDD): MDD = {

      val idn = id(n)
      if (common(idn) == null) {
        val oldTrie = n.traverseST.toSeq
        val newTrie = for ((i, m) <- oldTrie) yield {
          i -> step2(m)
        }
        val same = (oldTrie, newTrie).zipped.forall {
          case ((_, m1), (_, m2)) => m1 eq m2
        }
        if (same) {
          common(idn) = n
        } else {
          common(idn) = MDD(newTrie)
        }
      }
      common(idn)

    }

    step2(this)

  }

  def traverseST: Traversable[(Int, MDD)] = new Traversable[(Int, MDD)] {
    def foreach[A](f: ((Int, MDD)) => A) {
      forSubtries { (i, mdd) => f((i, mdd)); true }
    }
  }

  def contains(t: Array[Int], i: Int = 0): Boolean

  def findSupport(scope: Array[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int = 0, cache: IdSet[MDD] = new IdSet): Option[Array[Int]]

  def edges(cache: IdSet[MDD] = new IdSet): Int

  def vertices(cache: IdSet[MDD] = new IdSet): Int = {
    cache.onceOrElse(this,
      if (this eq MDDLeaf) {
        1
      } else {
        1 + traverseST.map { case (_, m) => m.vertices(cache) }.sum
      },
      0)
  }

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int = 0, cache: IdMap[MDD, MDD] = new IdMap): MDD

  def supported(doms: Array[MiniSet], newDomains: Array[BitVector], depth: Int, l: SetWithMax, cache: IdSet[MDD] = new IdSet): Unit

  final def lambda(map: IdMap[MDD, BigInt] = new IdMap): BigInt = {
    if (this eq MDDLeaf) {
      BigInt(1)
    } else {
      map.getOrElseUpdate(this, traverseST.map { case (_, m) => m.lambda(map) }.sum)
    }
  }

  def depth(map: IdMap[MDD, Option[Int]] = new IdMap): Option[Int] = {
    map.getOrElseUpdate(this, {
      traverseST
        .flatMap { case (_, m) => m.depth(map) }
        .reduceOption(_ max _)
        .map(1 + _)
    })
  }

  def forSubtries(f: (Int, MDD) => Boolean): Boolean

  override def isEmpty: Boolean

  override def equals(o: Any): Boolean = o match {
    case MDDLeaf => this eq MDDLeaf
    case t: MDD => {
      val t1 = t.traverseST.toMap
      val t2 = traverseST.toMap
      MDD.same(t1, t2)
    }
    case _ => false
  }

  def subMDD(i: Int): MDD

  override def toString = s"MDD ${System.identityHashCode(this)}, ${edges(new IdSet)} edges, ${vertices(new IdSet)} vertices, ${lambda(new IdMap)} tuples"

  def nodes(map: IdSet[MDD] = new IdSet): IdSet[MDD] = {
    if (!map.contains(this)) {
      map.put(this)
      for (node <- traverseST) {
        node._2.nodes(map)
      }
    }
    map
  }

  def union(mdd: MDD, cache: BIdMap[MDD, MDD, MDD] = new BIdMap): MDD = {
    cache.getOrElseUpdate(this, mdd, {
      val thisTrie = traverseST.toMap
      val otherTrie = mdd.traverseST
      val unionTrie = thisTrie ++ otherTrie.map { case (i, m) =>
        i -> thisTrie.getOrElse(i, MDD0).union(m, cache)
      }
      MDD(unionTrie.toSeq)
    })
  }

  def intersect(mdd: MDD, cache: BIdMap[MDD, MDD, MDD] = new BIdMap): MDD = {
    cache.getOrElseUpdate(this, mdd, {
      val interTrie = mdd.traverseST
        .map { case (i, m) =>
          i -> this.subMDD(i).intersect(m, cache)
        }
        .filter(_._2.nonEmpty)
      MDD(interTrie.toSeq)
    })
  }

  def project(c: Set[Int], k: Int = 0, mdds: IdMap[MDD, MDD] = new IdMap): MDD = {
    mdds.getOrElseUpdate(this, {
      if (c(k)) {
        val newTrie = traverseST.map {
          case (i, m) => i -> m.project(c, k + 1, mdds)
        }
        MDD(newTrie.toSeq)
      } else {
        traverseST
          .map(e => e._2.project(c, k + 1, mdds))
          .reduceLeft(_ union _)
      }
    })
  }

  def insertDim(pos: Int, domain: Iterable[Int], mdds: IdMap[MDD, MDD] = new IdMap): MDD = {
    mdds.getOrElseUpdate(this, {
      if (pos == 0) {
        MDD(domain.map(i => i -> this).toSeq)
      } else {
        val trie = traverseST
          .map(e => e._1 -> e._2.insertDim(pos - 1, domain, mdds))
        MDD(trie.toSeq)
      }
    })
  }

  protected def checkSup(domains: Array[MiniSet], p: Int, i: Int, index: Int, next: MDD, support: Array[Int], depth: Int, cache: IdSet[MDD]) = {
    val ok =
      if (p == depth) {
        i == index
      } else {
        domains(depth).present(index)
      }
    if (ok) {
      support(depth) = index
      next.findSupport(domains, p, i, support, depth + 1, cache)
    } else {
      None
    }
  }

}

final class MDD1(private val child: MDD, private val index: Int) extends MDD {
  assert(child.nonEmpty)

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

  def findSupport(scope: Array[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int, ts: IdSet[MDD]) = {
    ts.onceOrElse(
      this,
      checkSup(scope, p, i, index, child, support, depth, ts),
      None)
  }

  def supported(doms: Array[MiniSet], newDomains: Array[BitVector], depth: Int, l: SetWithMax, ts: IdSet[MDD]): Unit = {
    ts.once(
      this,
      if (depth <= l.max) {
        newDomains(depth) += index
        if (newDomains(depth).cardinality == doms(depth).size) l -= depth
        child.supported(doms, newDomains, depth + 1, l, ts)
      }
    )
  }

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int = 0, ts: IdMap[MDD, MDD]): MDD =
    if (modified.isEmpty) {
      this
    } else {
      ts.getOrElseUpdate(this, {
        val nC =
          if (modified.head == depth) {
            // Some change at this level
            if (doms(depth).present(index)) {
              child.filterTrie(doms, modified.tail, depth + 1, ts)
            } else {
              MDD0
            }
          } else {
            // No change at this level (=> no need to call f())
            child.filterTrie(doms, modified, depth + 1, ts)
          }

        if (nC eq MDD0) {
          MDD0
        } else if (nC eq child) {
          this
        } else {
          new MDD1(nC, index)
        }
      })
    }

  def iterator = child.iterator.map(index +: _)

  def edges(cache: IdSet[MDD]): Int = cache.onceOrElse(this, 1 + child.edges(cache), 0)

  override def isEmpty = false

  def subMDD(i: Int) = if (index == i) child else MDD0
}

final class MDD2(
                  private val left: MDD, private val leftI: Int,
                  private val right: MDD, private val rightI: Int) extends MDD {
  assert(right.nonEmpty)
  assert(left.nonEmpty)

  def forSubtries(f: (Int, MDD) => Boolean): Boolean = {
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

  def supported(doms: Array[MiniSet], newDoms: Array[BitVector], depth: Int, l: SetWithMax, ts: IdSet[MDD]): Unit =
    ts.once(
      this,
      if (depth <= l.max) {
        newDoms(depth) += leftI
        newDoms(depth) += rightI
        if (newDoms(depth).cardinality == doms(depth).size) l -= depth
        left.supported(doms, newDoms, depth + 1, l, ts)
        right.supported(doms, newDoms, depth + 1, l, ts)
      })

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[MDD, MDD]): MDD =
    if (modified.isEmpty) {
      this
    } else ts.getOrElseUpdate(this, {
      var nL: MDD = null
      var nR: MDD = null

      if (modified.head == depth) {
        // Some change at this level
        nL = filteredTrie(doms, modified.tail, depth, left, leftI, ts)
        nR = filteredTrie(doms, modified.tail, depth, right, rightI, ts)
      } else {
        // No change at this level (=> no need to check presence)
        nL = left.filterTrie(doms, modified, depth + 1, ts)
        nR = right.filterTrie(doms, modified, depth + 1, ts)
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

    })

  @inline
  private def filteredTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, t: MDD, i: Int, ts: IdMap[MDD, MDD]) = {
    if (doms(depth).present(i)) {
      t.filterTrie(doms, modified, depth + 1, ts)
    } else {
      MDD0
    }
  }

  def iterator: Iterator[Seq[Int]] =
    left.iterator.map(leftI +: _) ++ right.iterator.map(rightI +: _)

  def edges(ts: IdSet[MDD]): Int = ts.onceOrElse(this, 2 + left.edges(ts) + right.edges(ts), 0)

  override def isEmpty = false

  def findSupport(scope: Array[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int, ts: IdSet[MDD]) =
    ts.onceOrElse(
      this,
      checkSup(scope, p, i, leftI, left, support, depth, ts).orElse(
        checkSup(scope, p, i, rightI, right, support, depth, ts)),
      None)

  def subMDD(i: Int) = i match {
    case `leftI` => left
    case `rightI` => right
    case _ => MDD0
  }
}

final class MDDn(private val trie: Array[MDD]) extends MDD {

  assert(indicesIterator.nonEmpty)
  assert(trie.forall(m => (m eq null) || m.nonEmpty))

  def forSubtries(f: (Int, MDD) => Boolean): Boolean = {
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

  private def isEmpty(t: MDD) = t eq null

  def supported(doms: Array[MiniSet], newDomains: Array[BitVector], depth: Int, l: SetWithMax, ts: IdSet[MDD]): Unit =
    ts.once(this, {

      forIndices { i =>
        newDomains(depth) += i
      }

      if (newDomains(depth).cardinality == doms(depth).size) l -= depth

      if (depth <= l.max) {
        forIndices { i =>
          trie(i).supported(doms, newDomains, depth + 1, l, ts)
        }
      }
    })

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[MDD, MDD]): MDD =
    if (modified.isEmpty) {
      this
    } else ts.getOrElseUpdate(this, {

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

    })

  def iterator = indicesIterator.map(i => (trie(i), i)) flatMap {
    case (t, i) => t.iterator map (i +: _)
  }

  def indicesIterator = new Traversable[Int] {
    def foreach[U](f: Int => U): Unit = forIndices(f)
  }
    .toIterator

  private def forIndices[U](f: Int => U): Unit = {
    for (i <- 0 until trie.length) {
      if (!isEmpty(trie(i))) {
        f(i)
      }
    }
  }

  def edges(ts: IdSet[MDD]): Int = ts.onceOrElse(this, {
    var e = 0
    forIndices { i =>
      e += 1 + trie(i).edges(ts)
    }
    e
  }, 0)

  override def isEmpty = false

  def findSupport(scope: Array[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int, ts: IdSet[MDD]): Option[Array[Int]] =
    ts.onceOrElse(
      this,
      if (depth == p) {
        if (i >= trie.length || isEmpty(trie(i))) {
          None
        } else {
          support(depth) = i
          trie(i).findSupport(scope, p, i, support, depth + 1, ts)
        }
      } else {

        forIndices { index =>
          if (scope(depth).present(index)) {
            support(depth) = index
            val s = trie(index).findSupport(scope, p, i, support, depth + 1, ts)
            if (s.nonEmpty) return s
          }
        }
        None
      },
      None)

  def subMDD(i: Int) = {
    if (i >= trie.length || isEmpty(trie(i))) {
      MDD0
    } else {
      trie(i)
    }
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

  private def filteredTrie(ts: IdMap[MDD, MDD], doms: Array[MiniSet], modified: List[Int], depth: Int): Array[MDD] = {

    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)

    forIndices { i =>
      doms(depth).present(i) && {
        val uT = trie(i).filterTrie(doms, modified, depth + 1, ts)
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

  private def passedTrie(ts: IdMap[MDD, MDD], doms: Array[MiniSet], modified: List[Int], depth: Int): Array[MDD] = {
    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)

    forIndices { i =>
      val nT = trie(i).filterTrie(doms, modified, depth, ts)
      if (nT eq MDD0) {
        false
      } else {
        newTrie(i) = nT
        true
      }
    }
    newTrie
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

object MDDLeaf extends MDD {
  override lazy val hashCode = 0

  //override def size = 1

  override def id = 0

  //override def reduce(mdds: collection.mutable.Map[Map[Int, MDD], MDD]) = this
  def contains(tuple: Array[Int], i: Int) = true

  def iterator = Iterator(Nil)

  def edges(cache: IdSet[MDD]) = 0

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[MDD, MDD]) = {
    assert(modified.isEmpty)
    this
  }

  def supported(doms: Array[MiniSet], newDoms: Array[BitVector], depth: Int, l: SetWithMax, ts: IdSet[MDD]) = {
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

  def findSupport(scope: Array[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int, ts: IdSet[MDD]) =
    Some(support)

  override def equals(o: Any) = o match {
    case r: AnyRef => r eq MDDLeaf
    case _ => false
  }

  def subMDD(i: Int) = MDDLeaf

  override def union(m: MDD, cache: BIdMap[MDD, MDD, MDD]) = this

  override def intersect(m: MDD, cache: BIdMap[MDD, MDD, MDD]) = m

  override def project(c: Set[Int], k: Int, cache: IdMap[MDD, MDD]) = this

  override def insertDim(pos: Int, domain: Iterable[Int], mdds: IdMap[MDD, MDD]): MDD = {
    require(pos == 0)
    mdds.getOrElseUpdate(this, MDD(domain.map(i => i -> this).toSeq))
  }

  override def nodes(n: IdSet[MDD]): IdSet[MDD] = {
    n.put(this)
    n
  }

  override def depth(cache: IdMap[MDD, Option[Int]]) = Some(0)
}

final object MDD0 extends MDD {
  override def id = throw new UnsupportedOperationException

  def reduce(mdds: collection.mutable.Map[Seq[MDD], MDD]) = MDD0

  def contains(tuple: Array[Int], i: Int) = false

  def iterator = Iterator()

  def edges(cache: IdSet[MDD]) = 0

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[MDD, MDD]) = MDD0

  def supported(doms: Array[MiniSet], nd: Array[BitVector], depth: Int, l: SetWithMax, ts: IdSet[MDD]) = {
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

  def findSupport(scope: Array[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int, ts: IdSet[MDD]) =
    None

  def subMDD(i: Int) = MDD0

  override def union(m: MDD, mdds: BIdMap[MDD, MDD, MDD]) = m

  override def intersect(m: MDD, mdds: BIdMap[MDD, MDD, MDD]) = MDD0

  override def project(c: Set[Int], i: Int, cache: IdMap[MDD, MDD]) = MDD0
}


