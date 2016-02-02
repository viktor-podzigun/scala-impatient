
/**
 * Task 1:
 *  Write an object Conversions with methods inchesToCentimeters, gallonsToLiters,
 *  and milesToKilometers.
 */
object Conversions {

  def inchesToCentimeters(inches: Double): Double = inches * 2.54

  def gallonsToLiters(gallons: Double): Double = gallons * 3.785

  def milesToKilometers(miles: Double): Double = miles / 0.62137
}

/**
 * Task 2:
 *  The preceding problem wasn't very object-oriented. Provide a general super-class
 *  UnitConversion and define objects InchesToCentimeters, GallonsToLiters, and
 *  MilesToKilometers that extend it.
 */
abstract class UnitConversion {

  def convert(inches: Double): Double
}

object InchesToCentimeters extends UnitConversion {

  override def convert(inches: Double) = inches * 2.54
}

object GallonsToLiters extends UnitConversion {

  override def convert(gallons: Double) = gallons * 3.785
}

object MilesToKilometers extends UnitConversion {

  override def convert(miles: Double) = miles / 0.62137
}

/**
 * Task 3:
 *  Define an Origin object that extends java.awt.Point. Why is this not actually a good idea?
 *  (Have a close look at the methods of the Point class.)
 */
object Origin extends java.awt.Point {
  // Its not a good idea since java.awt.Point class is mutable.
}

package task0604 {

/**
 * Task 4:
 *
 * Define a Point class with a companion object so that you can construct Point
 * instances as Point(3, 4), without using new.
 */
class Point private(val x: Int, val y: Int) {
}

object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
}

}

/**
 * Task 5:
 *  Write a Scala application, using the App trait, that prints the command-line
 *  arguments in reverse order, separated by spaces. For example:
 *  <blockquote><code>
 *    scala Reverse Hello World
 *  </code></blockquote>
 *  should print
 *  <blockquote><code>
 *    World Hello
 *  </code></blockquote>
 */
object Reverse extends App {
  println(args.reverse.mkString(" "))
}

/**
 * Task 6:
 *  Write an enumeration describing the four playing card suits so that the toString method
 *  returns ♣, ♦, ♥, or ♠.
 */
object PlayingCard extends Enumeration {

  val Clubs = Value("♣")
  val Diamonds = Value("♦")
  val Hearts = Value("♥")
  val Spades = Value("♠")

  /**
   * Task 7:
   *   Implement a function that checks whether a card suit value from the preceding exercise
   *   is red.
   */
  def isRed(card: PlayingCard.Value): Boolean = {
    card == Hearts || card == Diamonds
  }
}

/**
 * Task 8:
 *   Write an enumeration describing the eight corners of the RGB color cube.
 *   As IDs, use the color values (for example, 0xff0000 for Red).
 */
object RGB extends Enumeration {

  val Black = Value(0x000000)
  val White = Value(0xffffff)
  val Red = Value(0xff0000)
  val Lime = Value(0x00ff00)
  val Blue = Value(0x0000ff)
  val Yellow = Value(0xffff00)
  val Cyan = Value(0x00ffff)
  val Magenta = Value(0xff00ff)
}
