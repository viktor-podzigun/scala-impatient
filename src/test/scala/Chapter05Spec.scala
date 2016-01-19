import Chapter05._
import org.scalatest.{FlatSpec, Matchers}

class Chapter05Spec extends FlatSpec with Matchers {

  "Chapter05.Counter01" should "not turn negative at Int.MaxValue" in {
    //when
    val counter = new Counter01(Int.MaxValue - 1)

    //then
    counter.current shouldBe (Int.MaxValue - 1)
    counter.increment() shouldBe Int.MaxValue
    a [IllegalStateException] should be thrownBy {
      counter.increment()
    }
  }

  "Chapter05.BankAccount02" should "deposit, withdraw and check balance" in {
    val account = new BankAccount02
    account.balance shouldBe 0

    account.deposit(10)
    account.balance shouldBe 10

    account.withdraw(7)
    account.balance shouldBe 3
  }
}
