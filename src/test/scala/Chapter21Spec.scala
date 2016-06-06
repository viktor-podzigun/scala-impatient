import Chapter21._
import org.scalatest.{FlatSpec, Matchers}

class Chapter21Spec extends FlatSpec with Matchers {

  "PercentAdder" should "define an operator `+%` and use implicit conversion" in {
    120 +% 10 shouldBe 132
  }
}
