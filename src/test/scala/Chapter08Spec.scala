import org.scalatest.{FlatSpec, Matchers}

class Chapter08Spec extends FlatSpec with Matchers {

  "CheckingAccount" should "charge $1 for every deposit and withdrawal" in {
    val account = new CheckingAccount(100)
    account.deposit(5) shouldBe 104
    account.withdraw(5) shouldBe 98
  }

  "SavingsAccount" should "earn interest every month" in {
    val account = new SavingsAccount(100)
    account.deposit(5) shouldBe 105
    account.withdraw(5) shouldBe 100
    account.deposit(3) shouldBe 103
    account.withdraw(3) shouldBe 99
    account.deposit(2) shouldBe 100
    account.earnMonthlyInterest()
    account.getBalance shouldBe 101
    account.withdraw(5) shouldBe 96
  }
}
