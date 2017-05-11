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
    ts.iterator.size shouldBe ts.lambda
  }

  it should "compute its size correctly" in {
    ts.lambda shouldBe BigInt(3)

    t.lambda shouldBe s.lambda

    u.lambda shouldBe t.lambda
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

    m.lambda shouldBe BigInt(6)
    m.edges(1) shouldBe 11

  }

  it should "have correct number of nodes" in {
    val m = MDD(Seq(
      Seq(2, 3, 2),
      Seq(1, 2, 1),
      Seq(1, 1, 1),
      Seq(1, 1, 3),
      Seq(3, 1, 1),
      Seq(3, 1, 3)))
      .reduce()

    val map = m.nodes(new IdMap())

    map.size shouldBe 7

    val m2 = m.filterTrie(5, Array(new MySet(1, 2, 3), new MySet(1), new MySet(1, 2, 3)), List(1), 0)

    m2.lambda shouldBe 4

    m2.nodes(map).size shouldBe 9

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
        MDD(trie.toMap)
      }
    }

    val mdd = complete(10, 10)
    mdd.lambda shouldBe BigInt(d).pow(k)
    mdd.edges(10) shouldBe d * k

    failAfter(Span(5, Seconds))(mdd.reduce)

  }
}
