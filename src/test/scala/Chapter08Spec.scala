import org.scalatest.{FlatSpec, Matchers}
import task0804.{Bundle, SimpleItem, Item}
import task0805.LabeledPoint

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

  "Item" should "has SimpleItem and Bundle implementations" in {
    val item: Item = new SimpleItem(500, "iPhone 5s")
    item.price shouldBe 500
    item.description shouldBe "iPhone 5s"

    val bundle: Bundle = new Bundle
    bundle.isInstanceOf[Item] shouldBe true
    bundle.price shouldBe 0
    bundle.description shouldBe ""

    bundle.addItem(item).addItem(new SimpleItem(700, "iPhone 6"))
    bundle.price shouldBe 1200
    bundle.description shouldBe "iPhone 5s\n\niPhone 6"
  }

  "Point" should "has LabeledPoint subclass" in {
    val point: LabeledPoint = new LabeledPoint("Black Thursday", 1929, 230.07)

    point.isInstanceOf[task0805.Point] shouldBe true
    point.label shouldBe "Black Thursday"
    point.x shouldBe 1929
    point.y shouldBe 230.07
  }
}
