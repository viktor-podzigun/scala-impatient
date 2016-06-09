import Chapter11.Fraction
import java.awt.Point
import scala.annotation.tailrec
import scala.io.StdIn
import scala.language.implicitConversions


object Chapter21 {

  /**
   * Task 1:
   *
   * How does `->` work? That is, how can `"Hello" -> 42` and `42 -> "Hello"` be pairs
   * `("Hello", 42)` and `(42, "Hello")`? Hint: `Predef.any2ArrowAssoc`.
   *
   * Solution:
   *
   * Currently, as of Scala 2.11.x, its implemented by using `implicit class ArrowAssoc` that
   * enriches any instance with `->` method.
   */

  /**
   * Task 2:
   *
   * Define an operator `+%` that adds a given percentage to a value. For example,
   * `120 +% 10` should be `132`. Hint: Since operators are methods, not functions,
   * you will also need to provide an `implicit`.
   */
  implicit class PercentAdder(private val value: Int) {

    def +%(percent: Int): Int = value + ((value * percent) / 100d).toInt
  }

  // as of Scala 2.11.x not needed any more, since we can use implicit class
  //implicit def int2PercentAdder(value: Int): PercentAdder = new PercentAdder(value)

  /**
   * Task 3:
   *
   * Define a `!` operator that computes the factorial of an integer. For example,
   * `5!` is `120`. You will need an enrichment class and an implicit conversion.
   */
  implicit class Int2Factorial(private val value: Int) {

    def ! : Int = {
      @tailrec
      def fact(acc: Int, n: Int): Int = {
        if (n == 1) acc
        else fact(acc * n, n - 1)
      }

      fact(1, value)
    }
  }

  /**
   * Task 4:
   *
   * Some people are fond of "fluent APIs" that read vaguely like English sentences.
   * Create such an API for reading integers, floating-point numbers, and strings from the console.
   * For example:
   * {{{
   * Read in aString askingFor "Your name" and
   *   anInt askingFor "Your age" and
   *   aDouble askingFor "Your weight".
   * }}}
   */
  sealed trait FluentFieldType

  object aString extends FluentFieldType

  object anInt extends FluentFieldType

  object aDouble extends FluentFieldType

  def Read: FluentReader = new FluentReader

  class FluentReader {

    private var data: List[(String, Any)] = List()

    private var nextType: FluentFieldType = aString

    def getData: List[(String, Any)] = data

    def in(fieldType: FluentFieldType): FluentReader = {
      nextType = fieldType
      this
    }

    def and(fieldType: FluentFieldType): FluentReader = in(fieldType)

    def askingFor(fieldName: String): FluentReader = {
      print(fieldName + ": ")

      val value = nextType match {
        case _: aString.type => StdIn.readLine()
        case _: anInt.type => StdIn.readInt()
        case _: aDouble.type => StdIn.readDouble()
      }

      data = data :+ (fieldName, value)
      this
    }
  }

  /**
   * Task 5:
   *
   * Provide the machinery that is needed to compute
   * {{{
   * smaller(Fraction(1, 7), Fraction(2, 9))
   * }}}
   * in Section 21.6, "Implicit Conversions with Implicit Parameters," on page 310.
   * Supply a `class RichFraction` that extends `Ordered[Fraction]`.
   */
  def smaller[T](a: T, b: T)(implicit order: T => Ordered[T]): T =
    if (order(a) < b) a
    else b

  implicit class RichFraction(self: Fraction) extends Ordered[Fraction] {

    override def compare(that: Fraction): Int = (self - that).num
  }

  /**
   * Task 6:
   *
   * Compare objects of the `class java.awt.Point` by lexicographic comparison.
   */
  implicit class LexicographicPointOrdering(self: Point) extends Ordered[Point] {

    override def compare(that: Point): Int =
      if (self.x == that.x) self.y - that.y
      else self.x - that.x
  }

  /**
   * Task 7:
   *
   * Continue the previous exercise, comparing two points according to their distance to
   * the origin. How can you switch between the two orderings?
   *
   * Solution:
   *
   * We can switch between the two orderings by importing appropriate `implicit class`:
   * {{{
   * import Chapter21.LexicographicPointOrdering
   * }}}
   * or
   * {{{
   * import Chapter21.DistancePointOrdering
   * }}}
   */
  implicit class DistancePointOrdering(self: Point) extends Ordered[Point] {

    override def compare(that: Point): Int = {
      val d1 = self.distance(0, 0)
      val d2 = that.distance(0, 0)

      if (d1 < d2) -1
      else if (d1 > d2) 1
      else 0
    }
  }
}
