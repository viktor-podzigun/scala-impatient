
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

/**
 * Task 4:
 *   Define a Point class with a companion object so that you can construct Point
 *   instances as Point(3, 4), without using new.
 */
class Point private(val x: Int, val y: Int) {
}

object Point {
  def apply(x: Int, y: Int) = new Point(x, y)
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
