import Chapter03._
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class Chapter03Spec extends FlatSpec with Matchers {

  "Chapter03" should "set a to an array of n random integers" in {
    val n = 5
    val a = randomIntArray(n)
    a.length should be (n)
    a.foreach(v => {
      v should be >= 0
      v should be < n
    })
  }

  it should "swap adjacent elements of integer array" in {
    val a = Array(1, 2, 3, 4, 5)
    val b = swapAdjacent(a)
    b shouldBe theSameInstanceAs(a)
    b shouldBe Array(2, 1, 4, 3, 5)

    swapAdjacent(Array()) shouldBe Array()
    swapAdjacent(Array(1)) shouldBe Array(1)
    swapAdjacent(Array(1, 2, 3, 4)) shouldBe Array(2, 1, 4, 3)
  }

  it should "swap adjacent elements of integer array and return new array" in {
    val a = Array(1, 2, 3, 4, 5)
    val b = swapAdjacentYield(a)
    b shouldNot be theSameInstanceAs a
    b shouldBe Array(2, 1, 4, 3, 5)

    swapAdjacentYield(Array()) shouldBe Array()
    swapAdjacentYield(Array(1)) shouldBe Array(1)
    swapAdjacentYield(Array(1, 2, 3, 4)) shouldBe Array(2, 1, 4, 3)
  }

  it should "produce new positives then negatives array" in {
    val a = Array(1, -2, 3, 0, 5, -4)
    val b = positivesThenNegatives(a)
    b shouldNot be theSameInstanceAs a
    b shouldBe Array(1, 3, 5, -2, 0, -4)

    positivesThenNegatives(Array()) shouldBe Array()
    positivesThenNegatives(Array(1)) shouldBe Array(1)
    positivesThenNegatives(Array(1, 2, 3, 4)) shouldBe Array(1, 2, 3, 4)
    positivesThenNegatives(Array(-1, -2, 0, -4)) shouldBe Array(-1, -2, 0, -4)
  }

  it should "compute the average of an Array[Double]" in {
    val a = Array(1.5, -1.5, 3, 0)
    val b = computeAverage(a)
    b shouldBe 0.75
  }

  it should "reverse sort Array[Int] in place" in {
    val a = Array(1, -1, 3, 0, 0, 2, 1)
    val b = reverseSortArray(a)
    b shouldBe theSameInstanceAs(a)
    b shouldBe Array(3, 2, 1, 1, 0, 0, -1)
  }

  it should "reverse sort ArrayBuffer[Int] in place" in {
    val a = ArrayBuffer(1, -1, 3, 0, 2, 1)
    val b = reverseSortArrayBuffer(a)
    b shouldBe theSameInstanceAs(a)
    b shouldBe Array(3, 2, 1, 1, 0, -1)
  }
}
