package mdd

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.util.Random


object MDDGenerator {
  /**
    * Robert Floyd algorithm to pick M elements from 1..N
    *
    * initialize set S to empty
    * for J := N-M + 1 to N do
    * T := RandInt(1, J)
    * if T is not in S then
    * insert T in S
    * else
    * insert J in S
    */

  def apply(d: Int, k: Int, lambda: Int, rand: Random): MDD = {
    var data: Set[Array[Int]] = Set()
    val n = BigInt(d).pow(k)

    for (j <- (n - lambda) until n) {
      val t = tupleSplit(randBigInt(j, rand), d, k)

      if (data(t)) {
        data += tupleSplit(j, d, k)
      } else {
        data += t
      }
    }

    MDD.fromSeq(data.toSeq)
  }

  /** max is inclusive */
  def randBigInt(max: BigInt, r: Random) = {
    require(max >= 0, max)
    if (max < Int.MaxValue) {
      BigInt(r.nextInt(max.toInt + 1))
    } else {
      var i: BigInt = null

      do {
        i = BigInt(max.bitLength, r) // r.nextInt(max.intValue)
      } while (i > max)

      i
    }
  }

  def tupleSplit(b: BigInt, d: Int, k: Int) = {
    val n = Array.ofDim[Int](k)
    var i = b
    var p = k - 1
    while (i > 0) {
      val (q, m) = i /% d
      n(p) = m.intValue
      i = q
      p -= 1
    }
    n
  }

  def giveStructure(mdd: MDD, q: Double, rand: Random, ts: Int) = {
    val existing = new mutable.HashMap[Int, ArrayBuffer[MDD]]()
    val cache = new IdMap[MDD, MDD]

    def giveStruct(n: MDD, k: Int): MDD = {
      if (n eq MDDLeaf) {
        n
      } else cache.getOrElseUpdate(n, {
        val e = existing.getOrElseUpdate(k, new ArrayBuffer())
        if (e.nonEmpty && rand.nextDouble() < q) {
          e(rand.nextInt(e.size))
        } else {
          val newMDD = MDD.fromTrie(
            n.children
              .map {
                case (i, m) => i -> giveStruct(m, k + 1)
              }
              .toSeq)

          val r = if (newMDD == n) n else newMDD
          e += r
          r
        }

      })
    }

    giveStruct(mdd, 0)
  }

}