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

  "ExprEvaluator2" should "handle ^ operator" in {
    //given
    val e = new ExprEvaluator2

    //when & then
    e.eval("3 - 4 * 5") shouldBe -17
    e.eval("3 - 8 / 4 * 5 + 3 % 4") shouldBe -4
    e.eval("4 ^ 2 ^ 3") shouldBe 65536
    e.eval("1 + 2 * 4 ^ 2 ^ 3 * 2 - 1") shouldBe 262144
  }

  "IntListParser" should "parse a list of integers into a List[Int]" in {
    //given
    val p = new IntListParser

    //when & then
    p.parse("()") shouldBe List()
    p.parse("(1)") shouldBe List(1)
    p.parse("(1, 23, -79)") shouldBe List(1, 23, -79)
  }
}
