import Chapter13._
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable

class Chapter13Spec extends FlatSpec with Matchers {

  "indexes" should "produce a mutable map of mutable set of indexes of all characters" in {
    //when
    val result: mutable.Map[Char, mutable.Set[Int]] = indexes("Mississippi")

    //then
    result('M') shouldBe mutable.Set(0)
    result('i') shouldBe mutable.Set(1, 4, 7, 10)
    result('s') shouldBe mutable.Set(2, 3, 5, 6)
    result('p') shouldBe mutable.Set(8, 9)
  }

  "indexes2" should "produce an immutable map of lists of indexes of all characters" in {
    //when
    val result: Map[Char, List[Int]] = indexes2("Mississippi")

    //then
    result('M') shouldBe List(0)
    result('i') shouldBe List(1, 4, 7, 10)
    result('s') shouldBe List(2, 3, 5, 6)
    result('p') shouldBe List(8, 9)
  }

  "removeAllZeroes" should "remove all zeroes from a linked list of integers" in {
    //when & then
    val list: mutable.LinkedList[Int] = mutable.LinkedList(1, 2, 3)
    val result: mutable.LinkedList[Int] = removeAllZeroes(list)
    result shouldBe theSameInstanceAs(list)

    removeAllZeroes(mutable.LinkedList(0, 1, 2, 3, 4)) shouldBe
      mutable.LinkedList(1, 2, 3, 4)

    removeAllZeroes(mutable.LinkedList(1, 2, 0, 3, 4)) shouldBe
      mutable.LinkedList(1, 2, 3, 4)

    removeAllZeroes(mutable.LinkedList(1, 2, 3, 4, 0)) shouldBe
      mutable.LinkedList(1, 2, 3, 4)

    removeAllZeroes(mutable.LinkedList(0, 1, 2, 0, 3, 4, 0)) shouldBe
      mutable.LinkedList(1, 2, 3, 4)

    removeAllZeroes(mutable.LinkedList(0, 0, 1, 2, 0, 0, 3, 0, 0, 4, 0, 0)) shouldBe
      mutable.LinkedList(1, 2, 3, 4)
  }
}
