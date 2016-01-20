import Chapter05._
import org.scalatest.{FlatSpec, Matchers}

class Chapter05Spec extends FlatSpec with Matchers {

  "Counter01" should "not turn negative at Int.MaxValue" in {
    //when
    val counter = new Counter01(Int.MaxValue - 1)

    //then
    counter.current shouldBe (Int.MaxValue - 1)
    counter.increment() shouldBe Int.MaxValue
    a [IllegalStateException] should be thrownBy {
      counter.increment()
    }
  }

  "BankAccount02" should "deposit, withdraw and check balance" in {
    val account = new BankAccount02
    account.balance shouldBe 0

    account.deposit(10)
    account.balance shouldBe 10

    account.withdraw(7)
    account.balance shouldBe 3
  }

  "Time03" should "check before" in {
    new Time03(0, 0).before(new Time03(0, 0)) shouldBe false
    new Time03(1, 1).before(new Time03(1, 1)) shouldBe false
    new Time03(1, 5).before(new Time03(1, 10)) shouldBe true
    new Time03(1, 5).before(new Time03(2, 10)) shouldBe true
    new Time03(1, 50).before(new Time03(1, 10)) shouldBe false
  }

  "Time04" should "check before" in {
    new Time04(0, 0).before(new Time04(0, 0)) shouldBe false
    new Time04(1, 1).before(new Time04(1, 1)) shouldBe false
    new Time04(1, 5).before(new Time04(1, 10)) shouldBe true
    new Time04(1, 5).before(new Time04(2, 10)) shouldBe true
    new Time04(1, 50).before(new Time04(1, 10)) shouldBe false
  }

  "Student05" should "call the JavaBeans getters and setters in Scala" in {
    val student = new Student05
    student.setName("Viktor")
    student.setId(23)

    student.getName shouldBe "Viktor"
    student.getId shouldBe 23
  }
}
