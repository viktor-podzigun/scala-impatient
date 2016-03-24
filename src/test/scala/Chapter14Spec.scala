import Chapter14._
import org.scalatest.{FlatSpec, Matchers}

class Chapter14Spec extends FlatSpec with Matchers {

  "swap" should "returns the pair with the components swapped" in {
    //given
    val pair = (1, 2)

    //when
    val result: (Int, Int) = swap(pair)

    //then
    result shouldBe (2, 1)
  }

  "swap2" should "swap the first two elements of an array" in {
    //when & then
    var arr: Array[Int] = Array()
    val r1: Array[Int] = swap2(arr)
    r1 shouldBe theSameInstanceAs(arr)
    r1 shouldBe Array()

    arr = Array(1)
    val r2 = swap2(arr)
    r2 shouldBe theSameInstanceAs(arr)
    r2 shouldBe Array(1)

    arr = Array(1, 2)
    val r3 = swap2(arr)
    r3 shouldBe theSameInstanceAs(arr)
    r3 shouldBe Array(2, 1)

    arr = Array(1, 2, 3, 4)
    val r4 = swap2(arr)
    r4 shouldBe theSameInstanceAs(arr)
    r4 shouldBe Array(2, 1, 3, 4)
  }

  "price" should "handle Multiple class items" in {
    //given
    val item = Bundle("Father's day special", 20.0,
      Article("Scala for the Impatient", 39.95),
      Bundle("Anchor Distilley Sampler", 10.0,
        Article("Old Potrero Straight Rye Whiskey", 79.95),
        Article("Junipero Gin", 32.95)),
      Multiple(10, Article("Blackwell Toaster", 29.95)))

    //when
    val result: Double = price(item)

    //then
    result.formatted("%.2f") shouldBe "422.35"
  }

  it should "be able to handle any items, such as bundles or multiples" in {
    //when & then
    price(Multiple(1,
      Multiple(2,
        Bundle("Black Friday special", 49.0,
          Multiple(3, Article("iPhone 5s", 549.99)))))
    ).formatted("%.2f") shouldBe "3201.94"
  }

  "leafSum" should "compute the sum of all elements in the leaves" in {
    //given
    val list: List[Any] = List(List(3, 8), 2, List(5))

    //when
    val result: Double = leafSum(list)

    //then
    result shouldBe 18
  }

  "Task6.leafSum" should "compute the sum of all elements in the BinaryTree" in {
    import Task6._

    //given
    val bt: Task6.BinaryTree = Node(Node(Leaf(3), Leaf(8)), Node(Leaf(2), Leaf(5)))

    //when
    val result: Int = leafSum(bt)

    //then
    result shouldBe 18
  }

  "Task7.leafSum" should "compute the sum of all elements in the multi-node tree" in {
    import Task7._

    //given
    val bt: Task7.BinaryTree = Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))

    //when
    val result: Int = leafSum(bt)

    //then
    result shouldBe 18
  }

  "eval" should "compute the value of the operator-node tree" in {
    import Task8._
    import Task8.Op._

    //given
    val bt: Task8.BinaryTree =
      Node(Plus, Node(Product, Leaf(3), Leaf(8)), Leaf(2), Node(Minus, Leaf(5)))

    //when
    val result: Int = eval(bt)

    //then
    result shouldBe 21
  }
}
