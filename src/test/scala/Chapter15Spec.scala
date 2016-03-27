import Chapter15._
import TestUtils.runApp
import org.scalatest.{FlatSpec, Matchers}

class Chapter15Spec extends FlatSpec with Matchers {

  "Task1" should "has four test cases that use the org.junit.Test annotation" in {
    //when
    val (exit, out, err) = runApp("org.junit.runner.JUnitCore", "Chapter15$Task1")

    //then
    exit shouldBe 0
    err shouldBe ""
    out should include ("OK (4 tests)")
  }

  "scalaSum" should "call Scala sum method with variable arguments from Java" in {
    //when & then
    Chapter15Task4.scalaSum(1, 2, 3) shouldBe 6
    sum(1, 2, 3, 4) shouldBe 10
  }
}
