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

    assertWordsMap(words)
  }

  it should "count words using immutable Map" in {
    val words: Map[String, Int] = countWordsImmutableMap()

    assertWordsMap(words)
  }

  it should "count words using SortedMap" in {
    val words: Map[String, Int] = countWordsSortedMap()

    assertWordsOrderedMap(words.toList)
  }

  it should "count words using TreeMap" in {
    val words: mutable.Map[String, Int] = countWordsTreeMap()

    assertWordsOrderedMap(words.toList)
  }

  private def assertWordsMap(words: collection.Map[String, Int]): Unit = {
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

  private def assertWordsOrderedMap(mapAsList: List[(String, Int)]): Unit = {
    mapAsList.size shouldBe 12
    mapAsList shouldBe List(
      "Simple" -> 1,
      "We" -> 1,
      "and" -> 1,
      "count" -> 1,
      "example" -> 1,
      "file" -> 2,
      "parse" -> 1,
      "text" -> 1,
      "the" -> 2,
      "will" -> 1,
      "with" -> 1,
      "words." -> 2)
  }
}
