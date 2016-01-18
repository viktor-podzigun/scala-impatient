import Chapter05._
import org.scalatest.{FlatSpec, Matchers}

class Chapter05Spec extends FlatSpec with Matchers {

  "Chapter05.Counter01" should "not turn negative at Int.MaxValue" in {
    //when
    val counter = new Counter01(Int.MaxValue - 1)

    //then
    counter.current shouldBe (Int.MaxValue - 1)
    counter.increment() shouldBe Int.MaxValue
    a [IllegalStateException] should be thrownBy {
      counter.increment()
    }
  }
}
