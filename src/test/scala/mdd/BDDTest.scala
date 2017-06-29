package mdd

import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.math.BigInt.int2bigInt
import scala.util.Random

final class BDDTest extends FlatSpec with Matchers with Inspectors {

  val t = BDD0 + List(1, 2, 3) + List(1, 3, 4) + List(1, 2, 5) + List(2, 3, 5)
  val s = BDD0 + List(1, 2, 5) + List(1, 3, 4) + List(1, 2, 3) + List(2, 3, 5)
  val u = BDD(MDD(
    Array(1, 2, 3),
    Array(1, 3, 4),
    Array(1, 2, 5),
    Array(2, 3, 5)))
  private val ts: BDD = BDD(MDD(Array(0, 0), Array(0, 1), Array(1, 0)))

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
    ts.iterator.size shouldBe ts.lambda()
  }

  it should "compute its size correctly" in {
    ts.lambda() shouldBe BigInt(3)

    t.lambda() shouldBe s.lambda()

    u.lambda() shouldBe t.lambda()
  }

  it should "reduce" in {
    val m0 = BDD(MDD(
      Array(2, 3, 2),
      Array(1, 2, 1),
      Array(1, 1, 1),
      Array(1, 1, 3),
      Array(3, 1, 1),
      Array(3, 1, 3)))

    // println(m0.edges(6))

    m0.lambda() shouldBe BigInt(6)

    val m = m0.reduce()

    withClue(m) {
      m.lambda() shouldBe BigInt(6)
      m.vertices() should be <= 13
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
      MDD.fromTrie((0 until d).map(i => i -> mdd(d, k - 1)))
    }
  }

  it should "reduce quickly" in {
    val d = 6
    val k = 7

    val m1 = mddl(d, k)

    m1.lambda() shouldBe BigInt(d).pow(k)
    m1.vertices() shouldBe (1 - BigInt(d).pow(k + 1)) / (1 - d) + 1

    //val t = System.nanoTime()
    val m = m1.reduce()
    //val e = System.nanoTime()

    //println((e - t) / 1e9)

    m.lambda() shouldBe BigInt(d).pow(k)
    m.vertices() shouldBe d * k + 2

    val m2 = mdd(d, k)
    m2.lambda() shouldBe BigInt(d).pow(k)
    m2.edges() shouldBe (1 - BigInt(d).pow(k + 1)) / (1 - d) - 1

    //val t2 = System.nanoTime()
    val m3 = m2.reduce()
    //val e2 = System.nanoTime()

    //println((e2 - t2) / 1e9)

    m3.lambda() shouldBe BigInt(d).pow(k)
    m3.edges() shouldBe d * k

  }

  it should "filter" in {
    val m = BDD(MDD(
      Array(2, 3, 2),
      Array(1, 2, 1),
      Array(1, 1, 1),
      Array(1, 1, 3),
      Array(3, 1, 1),
      Array(3, 1, 3)))
      .reduce()

    m.supported(Array.fill(3)(MySet(1, 2, 3)))

    //println(l1)

    val doms = Array[MiniSet](
      MySet(1 to 3: _*),
      MySet(1),
      MySet(1 to 3: _*))

    val n = m.filterTrie(doms, List(1))

    //println(n)

    n.supported(doms)


    //println(l)

  }

  it should "be smaller than array-based MDD" in {
    val rand = new Random(0)
    val mdd = MDDGenerator(15, 5, 600000, rand)
    val mddf = mdd.reduce()
    val mddl = BDD(mdd).reduce()
    val ef = mddf.edges()
    val el = mddl.vertices()
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

    mddr.edges() shouldBe mddl.vertices() - 2

    var doms = IndexedSeq.fill[MySet](k)(MySet(0 until d: _*))


    while (mddr.nonEmpty && mddl.nonEmpty) {
      mddr.lambda() shouldBe mddl.lambda()
      mddr.edges() should be >= mddl.vertices() - 2
      mddr should contain theSameElementsAs mddl

      val mod = List.tabulate(k)(i => i).filter(_ => rand.nextDouble() < .5)
      for (pos <- mod) {
        var newd = doms(pos)
        while (newd == doms(pos)) {
          newd = doms(pos).filter(_ => rand.nextDouble() > .1)
        }
        doms = doms.updated(pos, newd)
      }

      mddr = mddr.filterTrie(doms.toArray, mod)
      mddl = mddl.filterTrie(doms.toArray, mod)
    }

    assert(mddr.isEmpty)
    assert(mddl.isEmpty)
  }

  it should "convert MDD to BDD" in {
    val m = MDD(
      Array(2, 3, 2),
      Array(1, 2, 1),
      Array(1, 1, 1),
      Array(1, 1, 3),
      Array(3, 1, 1),
      Array(3, 1, 3))
      .reduce()

    val ml = BDD(m)

    m.edges() shouldBe ml.vertices() - 2
    m.toSet shouldBe ml.toSet

    val mlr = ml.reduce()
    mlr.edges() should be <= ml.edges()
  }

  it should "have correct number of nodes" in {
    val m = BDD(MDD(
      Array(2, 3, 2),
      Array(1, 2, 1),
      Array(1, 1, 1),
      Array(1, 1, 3),
      Array(3, 1, 1),
      Array(3, 1, 3)))
      .reduce()

    m.vertices() shouldBe 13

    val m2 = m.filterTrie(Array(MySet(1, 2, 3), MySet(1), MySet(1, 2, 3)), List(1))

    //println(m2.toList)

    m2.vertices() shouldBe 8

  }

  it should "have same numbers than in the paper" in {
    val m = MDD(
      Array(0, 0, 1),
      Array(0, 1, 1),
      Array(1, 1, 1)).reduce()

    m.vertices() shouldBe 5
    m.edges() shouldBe 6

    val b = BDD(m)

    b.vertices() shouldBe 8
    b.edges() shouldBe 12

    val b2 = b.reduce()

    b2.vertices() shouldBe 7
    b2.edges() shouldBe 10


  }
}
