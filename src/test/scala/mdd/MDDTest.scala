package mdd

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.concurrent.TimeLimits
import org.scalatest.prop.PropertyChecks
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FlatSpec, Inspectors, Matchers}

final class MDDTest extends FlatSpec with Matchers with Inspectors with TimeLimits {

  val t = MDD(Array(1, 2, 3), Array(1, 3, 4), Array(1, 2, 5), Array(2, 3, 5))
  val s = MDD(Array(1, 2, 5), Array(1, 3, 4), Array(1, 2, 3), Array(2, 3, 5))
  val u = MDD(
    Array(1, 2, 3),
    Array(1, 3, 4),
    Array(1, 2, 5),
    Array(2, 3, 5))
  private val ts: MDD = MDD(Array(0, 0), Array(0, 1), Array(1, 0))

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
    val m = MDD(
      Array(2, 3, 2),
      Array(1, 2, 1),
      Array(1, 1, 1),
      Array(1, 1, 3),
      Array(3, 1, 1),
      Array(3, 1, 3))
      .reduce()

    m.lambda() shouldBe BigInt(6)
    m.edges() shouldBe 11

  }

  it should "reduce quickly" in {

    def mdd(d: Int, k: Int): MDD = {
      if (k <= 0) {
        MDDLeaf
      } else {
        MDD.fromTrie((0 until d).map(i => i -> mdd(d, k - 1)))
      }
    }

    val d = 10
    val k = 7

    val m2 = mdd(d, k)
    //m2.lambda() shouldBe BigInt(d).pow(k)
    // m2.edges() shouldBe (1 - BigInt(d).pow(k + 1)) / (1 - d) - 1

    val t2 = System.nanoTime()
    val m3 = m2.reduce()
    val e2 = System.nanoTime()

    // logger.info("MDD:"  + (e2 - t2) / 1e9)

    m3.lambda() shouldBe BigInt(d).pow(k)
    m3.edges() shouldBe d * k
  }



  it should "reduce twice and return same instance" in {
    val m = MDD(
      Array(2, 3, 2),
      Array(1, 2, 1),
      Array(1, 1, 1),
      Array(1, 1, 3),
      Array(3, 1, 1),
      Array(3, 1, 3))
      .reduce()

    m.reduce() shouldBe theSameInstanceAs(m)


  }

  it should "have correct number of nodes" in {
    val m0 = MDD(
      Array(2, 3, 2),
      Array(1, 2, 1),
      Array(1, 1, 1),
      Array(1, 1, 3),
      Array(3, 1, 1),
      Array(3, 1, 3))

    m0.lambda() shouldBe 6
    m0.nodes() should have('size (9))
    m0.vertices() shouldBe 9
    m0.edges() shouldBe 13

    val m = m0.reduce()

    m.lambda() shouldBe 6
    m.nodes() should have('size (8))
    m.vertices() shouldBe 8
    m.edges() shouldBe 11

    val m2 = m.filterTrie(Array(MySet(1, 2, 3), MySet(1), MySet(1, 2, 3)), List(1))
    withClue(m2.iterator.mkString(", ")) {
      m2.lambda() shouldBe 4
      m2.nodes() should have('size (5))
      m2.edges() shouldBe 6
      m2.vertices() shouldBe 5
    }

    val m3 = m2.reduce()
    m3.lambda() shouldBe 4
    m3.nodes() should have('size (4))
    m3.vertices() shouldBe 4
    m3.edges() shouldBe 5

  }

  it should "reduce quickly large MDDs" in {
    val k = 10
    val d = 10

    def complete(k: Int, d: Int): MDD = {
      if (k <= 0) MDDLeaf
      else {
        val next = complete(k - 1, d)
        val trie = Seq.tabulate(d)(i => i -> next)
        MDD.fromTrie(trie)
      }
    }

    val mdd = complete(10, 10)
    mdd.lambda() shouldBe BigInt(d).pow(k)
    mdd.edges() shouldBe d * k

    failAfter(Span(20, Seconds))(mdd.reduce)

  }

  it should "compute unions correctly" in {
    val r2 = MDD(
      Array(1, 2, 3), Array(2, 5, 6), Array(3, 5, 5))

    r2.toSet ++ u should contain theSameElementsAs (u union r2)
  }

  it should "intersect" in {
    val r3 = MDD(Array(3, 5, 5), Array(1, 2, 2), Array(1, 2, 5), Array(2, 3, 5))
    val ri = u.intersect(r3)

    val nativeImpl = r3.filter(t => u.contains(t.toArray))

    nativeImpl should contain theSameElementsAs ri
  }

  it should "be projected" in {
    u.project(Set(0, 1)).asInstanceOf[Iterable[_]] should contain theSameElementsAs Seq(List(1, 2), List(1, 3), List(2, 3))
  }

  it should "add dimensions" in {

    val r2 = u.insertDim(3, List(2, 4, 6))
    r2.lambda() shouldBe u.lambda() * 3

  }

  it should "add starred tuples" in {
    val tuples = load("tuples4")
    val doms = IndexedSeq.fill(16)(-1 to 14)

    val mdd = MDD.fromStarred(tuples, doms)
    mdd.lambda() shouldBe BigInt("3251598930961498112")
  }

  it should "add many tuples" in {
    val tuples = load("tuples")
    val doms = projectDoms(tuples)
    val mdd = MDD.fromStarred(tuples, doms)
    mdd.lambda() shouldBe tuples.size

    val unstarred = tuples.map(_.map { case ValueStar(i) => i })
    val mdd2 = MDD.fromSeq(unstarred)
    mdd2.lambda() shouldBe tuples.size

    mdd should contain theSameElementsAs mdd2

    mdd.reduce().edges() shouldBe mdd2.reduce().edges()
  }

  private def load(r: String): Seq[IndexedSeq[Starrable]] = {
    val source = Option(getClass.getResource(r)).getOrElse(throw new IllegalStateException(s"Resource $r not found")).toURI
    scala.io.Source.fromFile(source, "UTF8")
      .getLines
      .map(_.split(",\\ *").toIndexedSeq.map {
        case "*" => Star
        case i => ValueStar(i.toInt)
      })
      .toSeq

  }

  private def projectDoms(tuples: Seq[IndexedSeq[Starrable]]): IndexedSeq[Seq[Int]] = {
    tuples.headOption.toIndexedSeq.flatMap { h =>
      Seq.tabulate(h.size) { i => tuples.map(_ (i)).collect { case ValueStar(i) => i }.distinct }
    }
  }

  it should "add many many tuples" in {
    val tuples = load("tuples2")
    val doms = projectDoms(tuples)

    val mdd = MDD.fromStarred(tuples, doms)

    mdd.lambda() shouldBe tuples.size

    val unstarred = tuples.map(_.map { case ValueStar(i) => i })
    val mdd2 = MDD.fromSeq(unstarred)
    mdd2.lambda() shouldBe tuples.size

  }

  it should "add many starred tuples" in {
    val tuples = load("tuples3")
    val doms = projectDoms(tuples)

    val mdd = MDD.fromStarred(tuples, doms)

    mdd.lambda() shouldBe BigInt("1284488745117187500")

    //println(mdd.edges())
    //println(mdd.reduce().edges())
    // mdd.reduce().lambda() shouldBe size
  }

  it should "merge MDD depths" in {
    val u = MDD(
      Array(1, 2, 2, 5),
      Array(1, 3, 3, 4),
      Array(1, 2, 5, 3),
      Array(2, 5, 5, 1),
      Array(2, 5, 5, 2))

    val expected = MDD(
      Array(1, 2, 5),
      Array(1, 3, 4),
      Array(2, 5, 1),
      Array(2, 5, 2)
    )

    val merged = u.merge(List(1, 2))

    merged should contain theSameElementsAs expected

    val u2 = MDD(
      Array(2, 1, 2, 5),
      Array(3, 1, 3, 4),
      Array(2, 1, 5, 3),
      Array(5, 2, 5, 1),
      Array(5, 2, 5, 2))

    val expected2 = MDD(
      Array(2, 1, 5),
      Array(3, 1, 4),
      Array(5, 2, 1),
      Array(5, 2, 2)
    )

    val merged2 = u2.merge(List(0, 2))

    merged2 should contain theSameElementsAs expected2
  }

  it should "have same result with fromTraversable and fromStarred" in {
    val gen = Gen.listOf(Gen.listOfN(2, Gen.chooseNum(-1000, 1000)))
    PropertyChecks.forAll(gen) { rel: List[List[Int]] =>
      val relation = rel.distinct.map(_.toIndexedSeq)

      val mdd1 = MDD.fromSeq(relation).reduce()
      mdd1.lambda() shouldBe relation.size

      val starred = relation.map(_.map(ValueStar(_)))
      val mdd2 = MDD.fromStarred(starred, projectDoms(starred)).reduce()
      mdd2.lambda() shouldBe relation.size

      mdd1.edges() shouldBe mdd2.edges()
    }
  }

}
