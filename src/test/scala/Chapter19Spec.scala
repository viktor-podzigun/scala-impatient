import Chapter19._
import org.scalatest.{FlatSpec, Matchers}

class Chapter19Spec extends FlatSpec with Matchers {

  "ExprEvaluator" should "handle / and % operations" in {
    //given
    val e = new ExprEvaluator

    //when & then
    e.eval("3 - 4 * 5") shouldBe -17
    e.eval("3 - 8 / 4 * 5 + 3 % 4") shouldBe -4
  }
}
