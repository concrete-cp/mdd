package mdd

import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{FlatSpec, Inspectors, Matchers}

final class MDDTest extends FlatSpec with Matchers with Inspectors with TimeLimits {

  val t = MDD0 + Array(1, 2, 3) + Array(1, 3, 4) + Array(1, 2, 5) + Array(2, 3, 5)
  val s = MDD0 + Array(1, 2, 5) + Array(1, 3, 4) + Array(1, 2, 3) + Array(2, 3, 5)
  val u = MDD(Seq(
    Seq(1, 2, 3),
    Seq(1, 3, 4),
    Seq(1, 2, 5),
    Seq(2, 3, 5)))
  private val ts: MDD = MDD(Seq(Seq(0, 0), Seq(0, 1), Seq(1, 0)))

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
    val m = MDD(Seq(
      Seq(2, 3, 2),
      Seq(1, 2, 1),
      Seq(1, 1, 1),
      Seq(1, 1, 3),
      Seq(3, 1, 1),
      Seq(3, 1, 3)))
      .reduce()

    m.lambda() shouldBe BigInt(6)
    m.edges() shouldBe 11

  }

  it should "reduce twice and return same instance" in {
    val m = MDD(Seq(
      Seq(2, 3, 2),
      Seq(1, 2, 1),
      Seq(1, 1, 1),
      Seq(1, 1, 3),
      Seq(3, 1, 1),
      Seq(3, 1, 3)))
      .reduce()

    m.reduce() shouldBe theSameInstanceAs(m)


  }

  it should "have correct number of nodes" in {
    val m0 = MDD(Seq(
      Seq(2, 3, 2),
      Seq(1, 2, 1),
      Seq(1, 1, 1),
      Seq(1, 1, 3),
      Seq(3, 1, 1),
      Seq(3, 1, 3)))

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
    m2.lambda() shouldBe 4
    m2.nodes() should have('size (5))
    m2.edges() shouldBe 6
    m2.vertices() shouldBe 5

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
        val trie = (0 until d).map {
          i => i -> next
        }
        MDD(trie)
      }
    }

    val mdd = complete(10, 10)
    mdd.lambda() shouldBe BigInt(d).pow(k)
    mdd.edges() shouldBe d * k

    failAfter(Span(5, Seconds))(mdd.reduce)

  }

  it should "compute unions correctly" in {
    val r2 = MDD(Seq(
      Seq(1, 2, 3), Seq(2, 5, 6), Seq(3, 5, 5)))

    r2.toSet ++ u should contain theSameElementsAs (u union r2)
  }

  it should "intersect" in {
    val r3 = MDD0 + List(3, 5, 5) + List(1, 2, 2) + List(1, 2, 5) + List(2, 3, 5)
    val ri = u.intersect(r3)

    val nativeImpl = r3.filter(t => u.contains(t.toArray))

    nativeImpl should contain theSameElementsAs ri
  }

  it should "be projected" in {
    u.project(Set(0, 1)).asInstanceOf[Iterable[_]] should contain theSameElementsAs (Seq(List(1, 2), List(1, 3), List(2, 3)))
  }

  it should "add dimensions" in {

    val r2 = u.insertDim(3, List(2, 4, 6))
    r2.lambda() shouldBe u.lambda() * 3

  }
}
