import Chapter12._
import org.scalatest.{FlatSpec, Matchers}

class Chapter12Spec extends FlatSpec with Matchers {

  "values" should "yield a collection of function inputs and outputs in a given range" in {
    //when
    val result: Seq[(Int, Int)] = values(x => x * x, -5, 5)

    //then
    result shouldBe Seq((-5, 25), (-4, 16), (-3, 9), (-2, 4), (-1, 1), (0, 0),
      (1, 1), (2, 4), (3, 9), (4, 16), (5, 25))
  }

  "largestElement" should "return the largest element of an array with reduceLeft" in {
    //given
    val arr = Array(5, 6, 1, 0, -2, 3, -1)

    //when
    val result: Int = largestElement(arr)

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

  "largest" should "return the largest value of a function within a given sequence of inputs" in {
    //when
    val result: Int = largest(x => 10 * x - x * x, 1 to 10)

    //then
    result shouldBe 25
  }

  "largestAt" should "return the input at which the output is largest" in {
    //when
    val result: Int = largestAt(x => 10 * x - x * x, 1 to 10)

    //then
    result shouldBe 5
  }

  "adjustToPair" should "return the function that operates on a pair" in {
    //when & then
    adjustToPair(_ * _)((6, 7)) shouldBe 42
  }

  "mapPairs" should "compute the sums of the elements in pairs" in {
    //given
    val pairs = (1 to 10) zip (11 to 20)

    //when
    val result: Seq[Int] = mapPairs(pairs, _ + _)

    result shouldBe Seq(12, 14, 16, 18, 20, 22, 24, 26, 28, 30)
  }
}
