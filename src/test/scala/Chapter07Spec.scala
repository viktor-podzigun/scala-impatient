import org.scalatest.{FlatSpec, Matchers}

class Chapter07Spec extends FlatSpec with Matchers {

  "puzzler" should "use a package com that isn’t at the top level" in {
    com.FromCom.value shouldBe 1

    import puzzler._
    com.FromCom.value shouldBe 21
  }
}
