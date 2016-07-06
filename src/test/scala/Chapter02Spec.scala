import Chapter02._
import org.scalatest.{FlatSpec, Matchers}

class Chapter02Spec extends FlatSpec with Matchers {

  "signum" should "return -1 if n < 0, 1 if n > 0 and 0 if n = 0" in {
    //when & then
    signum(-2) shouldBe -1
    signum(-1) shouldBe -1
    signum(0) shouldBe 0
    signum(1) shouldBe 1
    signum(2) shouldBe 1
  }
}
