package mdd

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by vion on 06/06/17.
  */
class SetWithMaxTest extends FlatSpec with Matchers {
  "SetWithMax" should "behave correctly" in {
    val s = new SetWithMax(10)
    s.max shouldBe 9
    s should contain theSameElementsAs (0 until 10)

    s.remove(8)
    s.max shouldBe 9
    s should contain theSameElementsAs (0 until 10).filter(_ != 8)

    s.remove(9)
    s.max shouldBe 7
    s should contain theSameElementsAs (0 until 8)

    s.clearFrom(5)
    s.max shouldBe 4
    s should contain theSameElementsAs (0 until 5)

    s.remove(4)
    s.max shouldBe 3
    s should contain theSameElementsAs (0 until 4)
  }
}
