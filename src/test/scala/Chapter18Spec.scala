import Chapter18._
import TestUtils.withOutput
import org.scalatest.{FlatSpec, Matchers}

class Chapter18Spec extends FlatSpec with Matchers {

  "Bug" should "has move, turn, and show methods" in {
    //given
    val bugsy = new Bug()

    //when
    val out = withOutput {
      bugsy.move(4).show().move(6).show().turn().move(5).show()
    }

    //then
    out shouldBe " 4 10 5"
  }

  it should "provide a fluent interface" in {
    //given
    val bugsy = new Bug() with FluentBug

    //when
    val out = withOutput {
      bugsy move 4 and show and then move 6 and show turn around move 5 and show
    }

    //then
    out shouldBe " 4 10 5"
  }

  "Book" should "provide a fluent interface" in {
    //given
    val book = new Book()

    //when
    book set Title to "Scala for the Impatient" set Author to "Cay Horstmann"

    //then
    book.getTitle shouldBe "Scala for the Impatient"
    book.getAuthor shouldBe "Cay Horstmann"
  }

  "Member.equals" should "return true if two members are in the same network" in {
    //given
    val network1 = new Network
    val member11 = new network1.Member
    val member12 = new network1.Member
    val network2 = new Network
    val member21 = new network2.Member

    //when & then
    member11.equals(member12) shouldBe true
    member11.equals(member21) shouldBe false
    member12.equals(member21) shouldBe false
  }

  "process" should "should accept members from any network" in {
    //given
    val network1 = new Network
    val member11 = new network1.Member
    val member12 = new network1.Member
    val network2 = new Network
    val member21 = new network2.Member

    //when & then
    process(member11, member12) shouldBe (member11, member12)
    process(member11, member21) shouldBe (member11, member21)
    process(member12, member21) shouldBe (member12, member21)
  }

  "processAny" should "should accept members from any network" in {
    //given
    val network1 = new Network
    val member11 = new network1.Member
    val member12 = new network1.Member
    val network2 = new Network
    val member21 = new network2.Member

    //when & then
    processAny(member11, member12) shouldBe (member11, member12)
    processAny(member11, member21) shouldBe (member11, member21)
    processAny(member12, member21) shouldBe (member12, member21)
  }

  "processSame" should "should accept members from the same network" in {
    //given
    val network1 = new Network
    val member11 = new network1.Member
    val member12 = new network1.Member
    val network2 = new Network
    val member21 = new network2.Member

    //when & then
    processSame(member11, member12) shouldBe (member11, member12)
    //processSame(member11, member21) shouldBe (member11, member21) // Error
    //processSame(member12, member21) shouldBe (member12, member21) // Error
  }
}
