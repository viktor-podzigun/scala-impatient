import Chapter16._
import org.scalatest.{FlatSpec, Matchers}

class Chapter16Spec extends FlatSpec with Matchers {

  "task1" should "check results" in {
    //when & then
    <fred/>(0).toString() shouldBe "<fred/>"
    <fred/>(0)(0).toString() shouldBe "<fred/>"
    <fred/>(0)(0)(0).toString() shouldBe "<fred/>"
    <fred/>(0)(0)(0)(0).toString() shouldBe "<fred/>"
  }
}
