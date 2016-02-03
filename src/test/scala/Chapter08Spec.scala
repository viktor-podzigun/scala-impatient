import org.scalatest.{FlatSpec, Matchers}
import task0804.{Bundle, Item, SimpleItem}
import task0805.{LabeledPoint, Point}
import task0806.{Circle, Rectangle, Shape}
import task0807.Square
import task0808.{Person, SecretAgent}

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

  "Shape06" should "has Rectangle and Circle subclasses" in {
    val circle: Shape = new Circle(new Point(1, 2), 3)
    val circleCenter: Point = circle.centerPoint
    circleCenter.x shouldBe 1
    circleCenter.y shouldBe 2

    val rectangle: Shape = new Rectangle(1, 2, 3, 4)
    val rectangleCenter: Point = rectangle.centerPoint
    rectangleCenter.x shouldBe ((1 + 3) / 2)
    rectangleCenter.y shouldBe ((2 + 4) / 2)
  }

  "Square" should "extend java.awt.Rectangle and has three constructors" in {
    import java.awt.Rectangle

    val square1: Rectangle = new Square(1, 2, 3)
    square1.x shouldBe 1
    square1.y shouldBe 2
    square1.width shouldBe 3
    square1.height shouldBe 3

    val square2: Rectangle = new Square(4)
    square2.x shouldBe 0
    square2.y shouldBe 0
    square2.width shouldBe 4
    square2.height shouldBe 4

    val square3: Rectangle = new Square()
    square3.x shouldBe 0
    square3.y shouldBe 0
    square3.width shouldBe 0
    square3.height shouldBe 0
  }

  "SecretAgent" should "extend Person and hide name" in {
    val person: Person = new SecretAgent("Bond")
    person.name shouldBe "secret"
    person.toString shouldBe "secret"
  }
}
