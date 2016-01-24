import org.scalatest.{FlatSpec, Matchers}

class Chapter07Spec extends FlatSpec with Matchers {

  "puzzler" should "use a package com that isn’t at the top level" in {
    com.FromCom.value shouldBe 1

    import puzzler._
    com.FromCom.value shouldBe 21
  }

  "random" should "has nextInt, nextDouble, and setSeed functions" in {
    random.nextInt() shouldBe 1013904223
    random.nextDouble() shouldBe (((1013904223 * 1664525) + 1013904223)/(Int.MaxValue + 1.0))

    random.setSeed(1)
    random.nextInt() shouldBe 1015568748
  }
}
