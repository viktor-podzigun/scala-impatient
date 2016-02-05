import scala.collection.mutable.ListBuffer

/**
 * Task 1:
 *
 * Extend the following `BankAccount` class to a `CheckingAccount` class that charges $1
 * for every deposit and withdrawal.
 * {{{
 * class BankAccount(initialBalance: Double) {
 *   private var balance = initialBalance
 *   def deposit(amount: Double) = { balance += amount; balance }
 *   def withdraw(amount: Double) = { balance -= amount; balance }
 * }
 * }}}
 */
class BankAccount(initialBalance: Double) {
  private var balance = initialBalance

  def deposit(amount: Double) = {
    balance += amount
    balance
  }

  def withdraw(amount: Double) = {
    balance -= amount
    balance
  }
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {

  private val charge: Double = 1

  override def deposit(amount: Double) = super.deposit(amount - charge)

  override def withdraw(amount: Double) = super.withdraw(amount + charge)
}

/**
 * Task 2:
 *
 * Extend the `BankAccount` class of the preceding exercise into a class `SavingsAccount`
 * that earns interest every month (when a method `earnMonthlyInterest` is called)
 * and has three free deposits or withdrawals every month. Reset the transaction
 * count in the `earnMonthlyInterest` method.
 */
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {

  private val monthlyInterest: Double = 0.01
  private val maxFreeTransactions: Int = 3

  private var transactionsCount: Int = 0
  private var balance: Double = initialBalance

  override def deposit(amount: Double) = {
    balance = super.deposit(amount - charge)
    balance
  }

  override def withdraw(amount: Double) = {
    balance = super.withdraw(amount + charge)
    balance
  }

  def getBalance: Double = {
    balance
  }

  def earnMonthlyInterest(): Unit = {
    transactionsCount = 0

    balance = super.deposit(balance * monthlyInterest)
  }

  private def charge: Double = {
    if (transactionsCount < maxFreeTransactions) {
      transactionsCount += 1
      return 0.0
    }

    1.0
  }
}

package task0803 {

/**
 * Task 3:
 *
 * Consult your favorite Java or C++ textbook that is sure to have an example
 * of a toy inheritance hierarchy, perhaps involving employees, pets, graphical
 * shapes, or the like. Implement the example in Scala.
 */
abstract class Shape {
  def draw(): Unit

  def erase(): Unit
}

class Circle extends Shape {

  override def draw(): Unit = {
    println("drawing Circle...")
  }

  override def erase(): Unit = {
    println("erasing Circle...")
  }
}

class Square extends Shape {

  override def draw(): Unit = {
    println("drawing Square...")
  }

  override def erase(): Unit = {
    println("erasing Square...")
  }
}

object Shapes extends App {

  cleanAndPaint(new Circle)
  cleanAndPaint(new Square)

  def cleanAndPaint(shape: Shape): Unit = {
    shape.erase()
    shape.draw()
  }
}

}

package task0804 {

/**
 * Task 4:
 *
 * Define an abstract class `Item` with methods `price` and `description`. A `SimpleItem`
 * is an item whose `price` and `description` are specified in the constructor. Take advantage
 * of the fact that a val can override a def. A `Bundle` is an item that contains other items.
 * Its price is the sum of the prices in the bundle. Also provide a mechanism for adding items
 * to the bundle and a suitable description method.
 */
abstract class Item {

  /** Price in minor units */
  def price: Int

  def description: String
}

class SimpleItem(override val price: Int, override val description: String) extends Item

class Bundle extends Item {

  private val items = new ListBuffer[Item]

  def addItem(item: Item): Bundle = {
    items += item
    this
  }

  override def price = items.foldLeft(0)((sum, item) => sum + item.price)

  override def description = items.map(_.description).mkString("\n\n")
}

}

package task0805 {

/**
 * Task 5:
 *
 * Design a class `Point` whose x and y coordinate values can be provided in a constructor.
 * Provide a subclass `LabeledPoint` whose constructor takes a `label` value and `x` and `y`
 * coordinates, such as
 * {{{
 *   new LabeledPoint("Black Thursday", 1929, 230.07)
 * }}}
 */
class Point(val x: Double, val y: Double)

class LabeledPoint(val label: String, x: Double, y: Double) extends Point(x, y)

}

package task0806 {

import task0805.Point

/**
 * Task 6:
 *
 * Define an abstract class `Shape` with an abstract method `centerPoint` and subclasses
 * `Rectangle` and `Circle`. Provide appropriate constructors for the subclasses and
 * override the `centerPoint` method in each subclass.
 */
abstract class Shape {
  def centerPoint: Point
}

class Circle(override val centerPoint: Point, val radius: Double) extends Shape

class Rectangle(val x1: Double, val y1: Double, val x2: Double, val y2: Double) extends Shape {

  override def centerPoint = new Point((x1 + x2) / 2, (y1 + y2) / 2)
}

}

package task0807 {

import java.awt.Rectangle

/**
 * Task 7:
 *
 * Provide a class `Square` that extends `java.awt.Rectangle` and has three constructors:
 * one that constructs a square with a given corner point and width,
 * one that constructs a square with corner (0, 0) and a given width,
 * and one that constructs a square with corner (0, 0) and width 0.
 */
class Square(x: Int = 0, y: Int = 0, width: Int = 0) extends Rectangle(x, y, width, width) {

  def this(width: Int) {
    this(0, 0, width)
  }
}

}

package task0808 {

/**
 * Task 8:
 *
 * Compile the `Person` and `SecretAgent` classes in Section 8.6, “Overriding Fields,” on page 91
 * and analyze the class files with `javap`.
 * How many name fields are there?
 * How many name getter methods are there?
 * What do they get?
 * (Hint: Use the -c and -private options.)
 */
class Person(val name: String) {
  override def toString = getClass.getName + "[name=" + name + "]"
}

/**
 * Solution:
 *
 * There are two name fields, one in Person class, and one in SecretAgent class.
 * There are two name getter methods, one in each class as well. The second one overrides the first.
 * They get/return corresponding name field from their class.
 *
 * Additionally, there is separate toString field and overrided toString method, which returns
 * that field.
 */
class SecretAgent(codename: String) extends Person(codename) {
  // Don’t want to reveal name...
  override val name = "secret"
  // ...or class name
  override val toString = "secret"
}

}

/**
 * Task 9:
 *
 * In the `Creature` class of Section 8.10, "Construction Order and Early Definitions,"
 * on page 94, replace `val range` with a `def`. What happens when you also use a
 * `def` in the `Ant` subclass? What happens when you use a `val` in the subclass?
 * Why?
 */
class Creature {
  def range: Int = 10
  val env: Array[Int] = new Array[Int](range)
}

class Ant extends Creature {
  override def range = 2
}

/**
 * Solution:
 *
 * When we use def in both superclass and subclass the array length is initialized with expected,
 * overrided value 2. This happens because there is no field to hold the value, and we don't have
 * initialization order problem.
 *
 * But when we use def in superclass and val in subclass initialization order problem is back
 * again. Since now the range value in subclass is backed by field and at the moment when
 * corresponding generated range getter method is called from superclass constructor the value is
 * not initialized yet and equals to 0 by default.
 */
object Creatures extends App {
  println(new Ant().env.length)
}

/**
 * Task 10:
 *
 * The file `scala/collection/immutable/Stack.scala` contains the definition
 * {{{
 * class Stack[A] protected (protected val elems: List[A])
 * }}}
 * Explain the meanings of the `protected` keywords. (Hint: Review the discussion
 * of private constructors in Chapter 5.)
 *
 *
 * Solution:
 *
 * The first `protected` keyword defines protected primary constructor, which is accessible only
 * from auxiliary constructor or from subclass primary constructor.
 * The second `protected` keyword defines protected field `elems` with corresponding protected
 * getter method, they are accessible within class and subclasses.
 */
