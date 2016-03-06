import Chapter12._
import org.scalatest.{FlatSpec, Matchers}

class Chapter12Spec extends FlatSpec with Matchers {

  "values" should "yield a collection of function inputs and outputs in a given range" in {
    //when
    val result = values(x => x * x, -5, 5)

    //then
    result shouldBe Seq((-5, 25), (-4, 16), (-3, 9), (-2, 4), (-1, 1), (0, 0),
      (1, 1), (2, 4), (3, 9), (4, 16), (5, 25))
  }

  "largestElement" should "return the largest element of an array with reduceLeft" in {
    //given
    val arr = Array(5, 6, 1, 0, -2, 3, -1)

    //when
    val result = largestElement(arr)

    //then
    result shouldBe 6
  }

  "factorial" should "be implemented using to and reduceLeft" in {
    //when & then
    factorial(-1) shouldBe 1
    factorial(0) shouldBe 1
    factorial(1) shouldBe 1
    factorial(5) shouldBe 120
  }

  "factorial2" should "be implemented using to and foldLeft" in {
    //when & then
    factorial2(-1) shouldBe 1
    factorial2(0) shouldBe 1
    factorial2(1) shouldBe 1
    factorial2(5) shouldBe 120
  }
}
