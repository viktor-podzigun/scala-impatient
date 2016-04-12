import Chapter17._
import org.scalatest.{FlatSpec, Matchers}

class Chapter17Spec extends FlatSpec with Matchers {

  "Pair.swap" should "return a new pair with the components swapped" in {
    //given
    val pair = Pair(1, "2")

    //when
    val result: Pair[String, Int] = pair.swap()

    //then
    result.first shouldBe "2"
    result.second shouldBe 1
  }

  "MutablePair.swap" should "swap the components of the mutable pair" in {
    //given
    val pair: MutablePair[Int] = new MutablePair(1, 2)

    //when
    pair.swap()

    //then
    pair.first shouldBe 2
    pair.second shouldBe 1
  }
}
