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

  "swap2" should "swap the first two elements of an array" in {
    //when & then
    var arr: Array[Int] = Array()
    val r1: Array[Int] = swap2(arr)
    r1 shouldBe theSameInstanceAs(arr)
    r1 shouldBe Array()

    arr = Array(1)
    val r2 = swap2(arr)
    r2 shouldBe theSameInstanceAs(arr)
    r2 shouldBe Array(1)

    arr = Array(1, 2)
    val r3 = swap2(arr)
    r3 shouldBe theSameInstanceAs(arr)
    r3 shouldBe Array(2, 1)

    arr = Array(1, 2, 3, 4)
    val r4 = swap2(arr)
    r4 shouldBe theSameInstanceAs(arr)
    r4 shouldBe Array(2, 1, 3, 4)
  }
}
