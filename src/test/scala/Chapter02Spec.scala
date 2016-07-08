import Chapter02._
import TestUtils.withOutput
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

  "task2" should "return Unit value" in {
    //when & then
    task2() shouldBe {}
  }

  "task3" should "return Unit as result of x = y = 1 expression" in {
    //when & then
    task3() shouldBe {}
  }

  "task4" should "implement simple Java for loop" in {
    //when
    val scalaOut = withOutput {
      task4()
    }

    //then
    scalaOut shouldBe withOutput {
      Chapter02Task4.javaForLoop()
    }
  }

  "countdown" should "print the numbers from n to 0" in {
    //when & then
    withOutput(countdown(3)) shouldBe """3
                                        |2
                                        |1
                                        |0
                                        |""".stripMargin

    withOutput(countdown(0)) shouldBe """0
                                        |""".stripMargin

    withOutput(countdown(-1)) shouldBe ""
  }

  "productLoop" should "compute product of Unicode codes letters in string using loop" in {
    //when & then
    productLoop("Hello") shouldBe 825152896
  }

  "productNoLoop" should "compute product of Unicode codes letters in string without loop" in {
    //when & then
    productNoLoop("Hello") shouldBe 825152896
  }

  "product" should "compute product of Unicode codes letters in string" in {
    //when & then
    product("Hello") shouldBe 825152896
  }
}
