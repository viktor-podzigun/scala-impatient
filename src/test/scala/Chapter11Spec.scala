import Chapter11._
import org.scalatest.{FlatSpec, Matchers}

class Chapter11Spec extends FlatSpec with Matchers {

  "Task1" should "evaluate expressions according to the precedence rules" in {
    //when & then
    (3 + 4 -> 5) shouldBe ((3 + 4) -> 5)
    (3 -> 4 + "5") shouldBe ((3 -> 4) + "5")
  }

  "Fraction" should "be normalized and has operations +, -, *, /" in {
    //when & then
    Fraction(15, -6).toString shouldBe "-5/2"
    (Fraction(1, 2) + Fraction(1, 2)).toString shouldBe "1/1"
    (Fraction(1, 2) - Fraction(1, 2)).toString shouldBe "0/1"
    (Fraction(1, 2) * Fraction(1, 2)).toString shouldBe "1/4"
    (Fraction(1, 2) / Fraction(1, 2)).toString shouldBe "1/1"
  }
}
