import org.scalatest.{FlatSpec, Matchers}

class Chapter17Spec extends FlatSpec with Matchers {

  "Pair.swap" should "return a new pair with the components swapped" in {
    import task1701._

    //given
    val pair: Pair[Int, String] = new Pair(1, "2")

    //when
    val result: Pair[String, Int] = pair.swap()

    //then
    result.first shouldBe "2"
    result.second shouldBe 1
  }

  "MutablePair.swap" should "swap the components of the mutable pair" in {
    import task1702._

    //given
    val pair: Pair[Int] = new Pair(1, 2)

    //when
    pair.swap()

    //then
    pair.first shouldBe 2
    pair.second shouldBe 1
  }

  "Task03.swap" should "take a pair and return a new pair with the components swapped" in {
    import task1701._

    //given
    val pair: Pair[Int, String] = new Pair(1, "2")

    //when
    val result: Pair[String, Int] = Chapter17Task03.swap(pair)

    //then
    result.first shouldBe "2"
    result.second shouldBe 1
  }

  "Task04.replaceFirst" should "take a pair and return a new pair with the components swapped" in {
    import task1704._

    //given
    val pair: Pair[Person] = new Pair[Person](new Person("First"), new Person("Second"))

    //when
    val result: Pair[Person] = pair.replaceFirst(new Student("newFirst"))

    //then
    result.first.name shouldBe "newFirst"
    result.second.name shouldBe "Second"
  }

  "Task06.middle" should "return the middle element from any Iterable[T]" in {
    //when & then
    Chapter17Task06.middle("World") shouldBe Some('r')
    Chapter17Task06.middle("Worl") shouldBe None
    Chapter17Task06.middle("Wor") shouldBe Some('o')
    Chapter17Task06.middle("Wo") shouldBe None
    Chapter17Task06.middle("W") shouldBe Some('W')
    Chapter17Task06.middle("") shouldBe None
    Chapter17Task06.middle(List(1, 2, 3)) shouldBe Some(2)
    Chapter17Task06.middle(Some(1)) shouldBe Some(1)
    Chapter17Task06.middle(None) shouldBe None
  }
}
