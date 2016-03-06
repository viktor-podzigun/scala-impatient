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
}
