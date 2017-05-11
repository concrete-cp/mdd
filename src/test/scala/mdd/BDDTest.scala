package mdd

import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.math.BigInt.int2bigInt
import scala.util.Random

final class BDDTest extends FlatSpec with Matchers with Inspectors {

  val t = BDD0 + List(1, 2, 3) + List(1, 3, 4) + List(1, 2, 5) + List(2, 3, 5)
  val s = BDD0 + List(1, 2, 5) + List(1, 3, 4) + List(1, 2, 3) + List(2, 3, 5)
  val u = BDD(Seq(
    List(1, 2, 3),
    List(1, 3, 4),
    List(1, 2, 5),
    List(2, 3, 5)))
  private val ts: BDD = BDD(Seq(List(0, 0), List(0, 1), List(1, 0)))

  "MDD" should "detect containment" in {
    ts should contain(Array(0, 1))
    ts should not contain (Array(1, 1))

    t should contain(Array(1, 3, 4))
    t should not contain (Array(1, 2, 4))

    forAll(t.toSeq.map(_.toArray)) { tuple =>
      s should contain(tuple)
      u should contain(tuple)
    }
  }

  it should "iterate over all tuples" in {
    ts.iterator.size shouldBe ts.lambda
  }

  it should "compute its size correctly" in {
    ts.lambda shouldBe BigInt(3)

    t.lambda shouldBe s.lambda

    u.lambda shouldBe t.lambda
  }

  it should "reduce" in {
    val m0 = BDD(Seq(
      List(2, 3, 2),
      List(1, 2, 1),
      List(1, 1, 1),
      List(1, 1, 3),
      List(3, 1, 1),
      List(3, 1, 3)))

    // println(m0.edges(6))

    m0.lambda shouldBe BigInt(6)

    val m = m0.reduce()

    withClue(m) {
      m.lambda shouldBe BigInt(6)
      m.vertices(new IdMap()) should be <= 13
    }

  }

  def mddl(d: Int, k: Int, i: Int = 0): BDD = {
    if (i >= d) {
      BDD0
    }
    else if (k <= 0) {
      BDDLeaf
    }
    else {
      new BDDNode(i, mddl(d, k - 1), mddl(d, k, i + 1))
    }
  }

  def mdd(d: Int, k: Int): MDD = {
    if (k <= 0) {
      MDDLeaf
    } else {
      MDD((0 until d).map(i => i -> mdd(d, k - 1)).toMap)
    }
  }

  it should "reduce quickly" in {
    val d = 6
    val k = 7

    val m1 = mddl(d, k)

    m1.lambda shouldBe BigInt(d).pow(k)
    m1.vertices(new IdMap()) shouldBe (1 - BigInt(d).pow(k + 1)) / (1 - d) + 1

    //val t = System.nanoTime()
    val m = m1.reduce()
    //val e = System.nanoTime()

    //println((e - t) / 1e9)

    m.lambda shouldBe BigInt(d).pow(k)
    m.vertices(new IdMap()) shouldBe d * k + 2

    val m2 = mdd(d, k)
    m2.lambda shouldBe BigInt(d).pow(k)
    m2.edges(1) shouldBe (1 - BigInt(d).pow(k + 1)) / (1 - d) - 1

    //val t2 = System.nanoTime()
    val m3 = m2.reduce()
    //val e2 = System.nanoTime()

    //println((e2 - t2) / 1e9)

    m3.lambda shouldBe BigInt(d).pow(k)
    m3.edges(2) shouldBe d * k

  }

  it should "filter" in {
    val m = BDD(Seq(
      List(2, 3, 2),
      List(1, 2, 1),
      List(1, 1, 1),
      List(1, 1, 3),
      List(3, 1, 1),
      List(3, 1, 3)))
      .reduce()

    val l1 = new SetWithMax(3)
    m.fillFound(1, { case t => false }, 0, l1)

    //println(l1)

    val doms = Array[MiniSet](
      new MySet(1 to 3: _*),
      new MySet(1),
      new MySet(1 to 3: _*))

    val n = m.filterTrie(2, doms, List(1), 0)

    //println(n)

    val l = new SetWithMax(3)
    val sup = Array.fill(3)(Set[Int]())
    n.fillFound(3, {
      case (p, i) =>
        sup(p) += i; sup(p).size == 3
    }, 0, l)

    //println(l)

  }

  it should "be smaller than array-based MDD" in {
    val rand = new Random(0)
    val mdd = MDDGenerator(15, 5, 600000, rand)
    val mddf = mdd.reduce()
    val mddl = BDD(mdd.map(_.toList)).reduce()
    val ef = mddf.edges(5)
    val el = mddl.vertices(new IdMap())
    ef should be >= el

  }

  it should "filter the same as MDD" in {
    val rand = new Random(0)
    val d = 5
    val k = 5
    val l = .28
    val lambda = (l * math.pow(d, k)).toInt
    var mddr = MDDGenerator(d, k, lambda, rand).reduce
    var mddl = BDD(mddr)

    mddr.edges(-2) shouldBe mddl.vertices(new IdMap()) - 2

    var doms = IndexedSeq.fill[MySet](k)(new MySet(0 until d: _*))

    var ts = 1
    while (mddr.nonEmpty && mddl.nonEmpty) {
      mddr.lambda shouldBe mddl.lambda
      mddr.edges(ts) should be >= mddl.vertices(new IdMap()) - 2
      mddr should contain theSameElementsAs mddl
      ts += 1
      val mod = List.tabulate(k)(i => i).filter(_ => rand.nextDouble() < .5)
      for (pos <- mod) {
        var newd = doms(pos)
        while (newd eq doms(pos)) {
          newd = doms(pos).filter(_ => rand.nextDouble() > .1)
        }
        doms = doms.updated(pos, newd)
      }

      mddr = mddr.filterTrie(ts, doms.toArray, mod, 0)
      mddl = mddl.filterTrie(ts, doms.toArray, mod, 0)
      ts += 1
    }

    assert(mddr.isEmpty)
    assert(mddl.isEmpty)
  }

  it should "convert MDD to BDD" in {
    val m = MDD(Seq(
      Seq(2, 3, 2),
      Seq(1, 2, 1),
      Seq(1, 1, 1),
      Seq(1, 1, 3),
      Seq(3, 1, 1),
      Seq(3, 1, 3)))
      .reduce()

    val ml = BDD(m)

    m.edges(5) shouldBe ml.vertices(new IdMap()) - 2
    m.toSet shouldBe ml.toSet

    val mlr = ml.reduce()
    mlr.edges(new IdMap()) should be <= ml.edges(new IdMap())
  }

  it should "have correct number of nodes" in {
    val m = BDD(Seq(
      List(2, 3, 2),
      List(1, 2, 1),
      List(1, 1, 1),
      List(1, 1, 3),
      List(3, 1, 1),
      List(3, 1, 3)))
      .reduce()

    m.vertices(new IdMap()) shouldBe 13

    val m2 = m.filterTrie(5, Array(new MySet(1, 2, 3), new MySet(1), new MySet(1, 2, 3)), List(1), 0)

    //println(m2.toList)

    m2.vertices(new IdMap()) shouldBe 8

  }

  it should "have same numbers than in the paper" in {
    val m = MDD(Seq(
      List(0, 0, 1),
      List(0, 1, 1),
      List(1, 1, 1))).reduce();

    m.vertices(new IdMap()) shouldBe 5;
    m.edges(1) shouldBe 6;

    val b = BDD(m)

    b.vertices(new IdMap()) shouldBe 8
    b.edges(new IdMap()) shouldBe 12

    val b2 = b.reduce()

    b2.vertices(new IdMap()) shouldBe 7
    b2.edges(new IdMap()) shouldBe 10


  }
}
