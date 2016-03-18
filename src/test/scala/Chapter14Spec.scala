import Chapter14._
import org.scalatest.{FlatSpec, Matchers}

class Chapter14Spec extends FlatSpec with Matchers {

  "swap" should "returns the pair with the components swapped" in {
    //given
    val pair = (1, 2)

    //when
    val result: (Int, Int) = swap(pair)

    //then
    result shouldBe (2, 1)
  }
}
