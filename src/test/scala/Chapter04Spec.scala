import Chapter04._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable

class Chapter04Spec extends FlatSpec with Matchers {

  "Chapter04" should "set a to an array of n random integers" in {
    val gizmos: Map[String, Int] = gizmosWithReducedPrice()
    gizmos("iPhone") shouldBe 540
    gizmos("iPad") shouldBe 450
    gizmos("MacBook Pro") shouldBe 1800
    gizmos("ScalaDays 2016 Berlin") shouldBe 675
  }

  it should "count words using mutable Map" in {
    val words: mutable.Map[String, Int] = countWordsMutableMap()
    words.size shouldBe 12
    words("Simple") shouldBe 1
    words("text") shouldBe 1
    words("file") shouldBe 2
    words("with") shouldBe 1
    words("example") shouldBe 1
    words("words.") shouldBe 2
    words("We") shouldBe 1
    words("will") shouldBe 1
    words("parse") shouldBe 1
    words("the") shouldBe 2
    words("and") shouldBe 1
    words("count") shouldBe 1
  }
}
