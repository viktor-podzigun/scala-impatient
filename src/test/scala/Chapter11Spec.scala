import Chapter11._
import org.scalatest.{FlatSpec, Matchers}

class Chapter11Spec extends FlatSpec with Matchers {

  "Task1" should "evaluate expressions according to the precedence rules" in {
    //when & then
    (3 + 4 -> 5) shouldBe ((3 + 4) -> 5)
    (3 -> 4 + "5") shouldBe ((3 -> 4) + "5")
  }
}
