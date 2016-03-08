import Chapter13._
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable

class Chapter13Spec extends FlatSpec with Matchers {

  "indexes" should "produce a map of the indexes of all characters" in {
    //when
    val result: mutable.Map[Char, mutable.Set[Int]] = indexes("Mississippi")

    //then
    result('M') shouldBe mutable.Set[Int](0)
    result('i') shouldBe mutable.Set[Int](1, 4, 7, 10)
    result('s') shouldBe mutable.Set[Int](2, 3, 5, 6)
    result('p') shouldBe mutable.Set[Int](8, 9)
  }
}
