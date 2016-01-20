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

  "Person06" should "turn negative ages in constructor to 0" in {
    new Person06(-5).age shouldBe 0
    new Person06(-1).age shouldBe 0
    new Person06(0).age shouldBe 0
    new Person06(1).age shouldBe 1
    new Person06(5).age shouldBe 5
  }

  "Person07" should "split name into firstName and lastName" in {
    val person = new Person07("Fred Smith")
    person.firstName shouldBe "Fred"
    person.lastName shouldBe "Smith"
    a [IllegalArgumentException] should be thrownBy {
      new Person07("SingleName")
    }
  }

  "Car08" should "provide 4 constructors" in {
    val car1 = new Car08("BMW", "X5")
    car1.manufacturer shouldBe "BMW"
    car1.modelName shouldBe "X5"
    car1.modelYear shouldBe -1
    car1.licensePlate shouldBe ""

    val car2 = new Car08("BMW", "X5", 2005)
    car2.manufacturer shouldBe "BMW"
    car2.modelName shouldBe "X5"
    car2.modelYear shouldBe 2005
    car2.licensePlate shouldBe ""

    val car3 = new Car08("BMW", "X5", "license")
    car3.manufacturer shouldBe "BMW"
    car3.modelName shouldBe "X5"
    car3.modelYear shouldBe -1
    car3.licensePlate shouldBe "license"

    val car4 = new Car08("BMW", "X5", 2005, "license")
    car4.manufacturer shouldBe "BMW"
    car4.modelName shouldBe "X5"
    car4.modelYear shouldBe 2005
    car4.licensePlate shouldBe "license"
  }
}
