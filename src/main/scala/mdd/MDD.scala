package mdd

import bitvectors.BitVector

import scala.util.hashing.MurmurHash3


object MDD {
  def apply(data: Traversable[Seq[Int]]): MDD = {
    data.headOption.map { h =>
      grouping(data, 0, h.size)
    }
      .getOrElse {
        MDD0
      }
    //
    //    data.foldLeft[MDD](MDD0)(
    //      (acc, tuple) => acc + tuple) //.reduce(new IdMap[Seq[MDD], MDD]())
  }

  def grouping(data: Traversable[Seq[Int]], i: Int, arity: Int): MDD = {
    if (i < arity) {
      val trie = data
        .groupBy(tuple => tuple(i))
        .mapValues(group => grouping(group, i + 1, arity))
      MDD(trie.toSeq)
    } else {
      MDDLeaf
    }
  }


  def fromStarred(data: Traversable[Seq[Seq[Int]]]): MDD = {
    val dat = data.toSeq

    dat.foldLeft[MDD](MDD0) { (acc, starTuple) =>
      acc.addStarred(starTuple)
    }
  }

  def newTrie(i: Int, v: MDD): (Int, Array[MDD]) = {
    (i, Array(v))
  }

  def apply(t: Seq[(Int, MDD)]): MDD = {
    assert(t.map(_._1).distinct.size == t.size)
    t match {
      case Seq() => MDD0
      case Seq((index, child)) => new MDD1(child, index)
      case Seq((i1, c1), (i2, c2)) => new MDD2(c1, i1, c2, i2)
      case e: Any => {
        val (offset, trie) = newTrie(e: _*)
        new MDDn(offset, trie)
      }
    }
  }

  def newTrie(t: (Int, MDD)*): (Int, Array[MDD]) = {
    val values = t.view.map(_._1)
    val offset = values.min
    val trie = new Array[MDD](values.max - offset + 1)
    for ((i, v) <- t) {
      trie(i - offset) = v
    }
    (offset, trie)
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
        }
        id
      }, i)
    }
  }

  def +(t: Seq[Int]): MDD = {
    val ta = t.toArray
    if (contains(ta)) MDD.this else addTrie(ta, 0)
  }

  def addStarred(starTuple: Seq[Seq[Int]], bIdMap: BIdMap[Seq[Seq[Int]], MDD, MDD] = new BIdMap()): MDD = {
    if (starTuple.isEmpty) {
      MDDLeaf
    } else bIdMap.getOrElseUpdate(starTuple, this, {
      val tail = starTuple.tail
      val trie = starTuple.head.foldLeft(traverseST.toMap) { (trie, v) =>
        val ns = trie.getOrElse(v, MDD0).addStarred(tail, bIdMap)
        trie.updated(v, ns)
      }

      MDD(trie.toSeq)
    })
  }

  @Deprecated
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
      forSubtries { (i, mdd) => f((i, mdd)) }
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

  def forSubtries(f: (Int, MDD) => Unit): Unit

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

  def merge(depths: List[Int], currentDepth: Int = 0, value: Integer = null, mdds: BIdMap[MDD, Integer, MDD] = new BIdMap()): MDD = {
    mdds.getOrElseUpdate(this, value, {
      if (depths.isEmpty) {
        this
      } else if (currentDepth == depths.head) {
        if (value == null) {
          val trie = traverseST
            .map { case (v, e) => v -> e.merge(depths.tail, currentDepth + 1, v, mdds) }
          MDD(trie.toSeq)
        } else {
          traverseST
            .filter { case (v, _) => value == v }
            .map { case (_, e) => e.merge(depths.tail, currentDepth + 1, value, mdds) }
            .foldLeft[MDD](MDD0)(_ union _)
        }

      } else {
        val trie = traverseST
          .map { case (v, e) => v -> e.merge(depths, currentDepth + 1, value, mdds) }

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

  def forSubtries(f: (Int, MDD) => Unit) = {
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

  def forSubtries(f: (Int, MDD) => Unit): Unit = {
    f(leftI, left)
    f(rightI, right)
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
          val (offset, newArray) = MDD.newTrie((leftI, left), (rightI, right), (v, MDD0.addTrie(t, i + 1)))
          new MDDn(offset, newArray)

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

final class MDDn(private val offset: Int, private val trie: Array[MDD]) extends MDD {

  assert(iterator.nonEmpty)
  assert(trie.forall(m => (m eq null) || m.nonEmpty))

  def addTrie(tuple: Array[Int], i: Int): MDD = {
    if (i >= tuple.length) {
      MDDLeaf
    } else {
      val v = tuple(i)
      val newOffset = math.min(offset, v)

      // Copy and shift
      val newTrie = new Array[MDD](math.max(v - newOffset, trie.length - newOffset + offset) + 1)
      for (i <- 0 until trie.length) {
        newTrie(i - newOffset + offset) = trie(i)
      }

      val p = v - newOffset

      if (isEmpty(newTrie(p))) {
        newTrie(p) = MDD0.addTrie(tuple, i + 1)
        new MDDn(newOffset, newTrie)
      } else {
        val ntv = newTrie(p).addTrie(tuple, i + 1)

        if (ntv eq newTrie(p)) {
          this
        } else {
          newTrie(p) = ntv
          new MDDn(newOffset, newTrie)
        }
      }

    }
  }

  def contains(tuple: Array[Int], i: Int): Boolean = {
    val v = tuple(i) - offset
    0 <= v && v < trie.length && !isEmpty(trie(v)) && trie(v).contains(tuple, i + 1)
  }

  def supported(doms: Array[MiniSet], newDomains: Array[BitVector], depth: Int, l: SetWithMax, ts: IdSet[MDD]): Unit =
    ts.once(this, {

      forValues { i =>
        newDomains(depth) += i
      }

      if (newDomains(depth).cardinality == doms(depth).size) l -= depth

      if (depth <= l.max) {
        forSubtriesNoOffset { (_, mdd) =>
          mdd.supported(doms, newDomains, depth + 1, l, ts)
        }
      }
    })

  private def forValues[U](f: Int => U): Unit = {
    for (i <- 0 until trie.length) {
      if (!isEmpty(trie(i))) {
        f(i + offset)
      }
    }
  }

  def forSubtriesNoOffset(f: (Int, MDD) => Unit): Unit = {
    var i = 0
    while (i < trie.length) {
      if (!isEmpty(trie(i))) {
        f(i, trie(i))
      }
      i += 1
    }
  }

  private def isEmpty(t: MDD) = t eq null

  def filterTrie(doms: Array[MiniSet], modified: List[Int], depth: Int, ts: IdMap[MDD, MDD]): MDD =
    if (modified.isEmpty) {
      this
    } else ts.getOrElseUpdate(this, {
      //println(depth + " : " + trie.zipWithIndex.mkString("[", ", ", "]") + " + " + offset)
      val newTrie =
        if (modified.head == depth) {
          // Some change at this level
          filteredTrie(ts, doms, modified.tail, depth)
        } else {
          // No change at this level (=> no need to call f())
          passedTrie(ts, doms, modified, depth)
        }
      //println(depth + " : " + newTrie.zipWithIndex.mkString("[", ", ", "]") + " + " + offset)
      if (same(newTrie, trie)) {
        this
      } else {
        newNode(newTrie)
      }

    })

  def iterator = traverseST.toIterator.flatMap {
    case (i, t) => t.iterator map (i +: _)
  }

  def edges(ts: IdSet[MDD]): Int = ts.onceOrElse(this, {
    var e = 0
    for ((_, mdd) <- traverseST) {
      e += 1 + mdd.edges(ts)
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

        forSubtries { (value, mdd) =>
          if (scope(depth).present(value)) {
            support(depth) = value
            val s = mdd.findSupport(scope, p, i, support, depth + 1, ts)
            if (s.nonEmpty) return s
          }
        }
        None
      },
      None)

  def forSubtries(f: (Int, MDD) => Unit): Unit = {
    var i = 0
    while (i < trie.length) {
      if (!isEmpty(trie(i))) {
        f(i + offset, trie(i))
      }
      i += 1
    }
  }

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
        new MDD1(t(i1), i1 + offset)
      } else {
        val i2 = i
        while (i >= 0 && isEmpty(t(i))) {
          i -= 1
        }
        if (i < 0) {
          new MDD2(t(i2), i2 + offset, t(i1), i1 + offset)
        } else {
          new MDDn(offset, t)
        }
      }
    }
  }

  private def filteredTrie(ts: IdMap[MDD, MDD], doms: Array[MiniSet], modified: List[Int], depth: Int): Array[MDD] = {

    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)

    forSubtriesNoOffset { (i, mdd) =>
      if (doms(depth).present(i + offset)) {
        val uT = mdd.filterTrie(doms, modified, depth + 1, ts)
        if (uT ne MDD0) {
          newTrie(i) = uT
        }
      }
    }

    newTrie
  }

  private def passedTrie(ts: IdMap[MDD, MDD], doms: Array[MiniSet], modified: List[Int], depth: Int): Array[MDD] = {
    val trie = this.trie
    val newTrie: Array[MDD] = new Array(trie.length)
    val nextDepth = depth + 1
    forSubtriesNoOffset { (i, mdd) =>
      val nT = mdd.filterTrie(doms, modified, nextDepth, ts)
      if (nT ne MDD0) {
        newTrie(i) = nT
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

  def forSubtries(f: (Int, MDD) => Unit): Unit = {
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

  def forSubtries(f: (Int, MDD) => Unit) = ()

  override def isEmpty = true

  def findSupport(scope: Array[MiniSet], p: Int, i: Int, support: Array[Int], depth: Int, ts: IdSet[MDD]) =
    None

  def subMDD(i: Int) = MDD0

  override def union(m: MDD, mdds: BIdMap[MDD, MDD, MDD]) = m

  override def intersect(m: MDD, mdds: BIdMap[MDD, MDD, MDD]) = MDD0

  override def project(c: Set[Int], i: Int, cache: IdMap[MDD, MDD]) = MDD0
}


