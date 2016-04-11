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
}
