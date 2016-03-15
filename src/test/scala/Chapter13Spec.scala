import Chapter13._
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable
import scala.io.Source

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

  "mapToValues" should "return collection of corresponding integer values from map" in {
    //given
    val coll = Array("Tom", "Fred", "Harry")
    val map = Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)

    //when
    val result = mapToValues(coll, map)

    //then
    result shouldBe Array(3, 5)
  }

  "collToString" should "works just like mkString, using reduceLeft" in {
    //when & then
    collToString(Nil) shouldBe ""
    collToString(Array(1)) shouldBe "1"
    collToString(Seq(1, 2, 3)) shouldBe "1, 2, 3"
    collToString(List("1", "2", "3")) shouldBe "1, 2, 3"
  }

  "reversList" should "revers the given list" in {
    //given
    val lst = List(1, 2, 3, 4, 5)

    //when
    val result: List[Int] = reversList(lst)

    //then
    result shouldBe List(5, 4, 3, 2, 1)
  }

  "multiply" should "apply Function.tupled to the multiplication function" in {
    //given
    val prices = List(1, 2, 3)
    val quantities = List(10, 20, 30)

    //when
    val result = multiply(prices, quantities)

    //then
    result shouldBe List(10, 40, 90)
  }

  "twoDimensionalArray" should "turn an array of Double values into a two-dimensional array" in {
    //given
    val arr = Array[Double](1, 2, 3, 4, 5, 6)
    val columns = 3

    //when
    val result: Array[Array[Double]] = twoDimensionalArray(arr, columns)

    //then
    result shouldBe Array[Array[Double]](Array(1, 2, 3), Array(4, 5, 6))
  }

  "getLetterFrequencyMap" should "return letter frequency map for the given files" in {
    //given
    val files = Array.fill(5)("/myfile.txt")

    //when
    val result: Map[Char, Int] = getLetterFrequencyMap(files)

    //then
    result shouldBe getTestFrequencyMap(files)
  }

  private def getTestFrequencyMap(files: Array[String]): Map[Char, Int] = {
    val result = new mutable.HashMap[Char, Int]()
    for (c <- Source.fromInputStream(getClass.getResourceAsStream(files.head))) {
      result(c) = result.getOrElse(c, 0) + 1
    }

    val multiplier = files.length
    for ((k, v) <- result) {
      result(k) = v * multiplier
    }

    result.toMap
  }
}
