import Chapter11.Fraction
import java.awt.Point
import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq
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

  /**
   * Task 8:
   *
   * Use the `implicitly` command in the REPL to summon the implicit objects described in
   * Section 21.5, "Implicit Parameters," on page 309 and
   * Section 21.6, "Implicit Conversions with Implicit Parameters," on page 310.
   * What objects do you get?
   *
   * Solution:
   *
   * To start the REPL console, start the following command in the project root directory:
   * {{{
   * activator console
   * }}}
   * Here is the REPL output:
   * {{{
   * scala> import Chapter21._
   * import Chapter21._
   *
   * scala> import Chapter11.Fraction
   * import Chapter11.Fraction
   *
   * scala> implicitly[Delimiters]
   * res0: Chapter21.Delimiters = Delimiters(<<,>>)
   *
   * scala> implicitly[Ordered[Fraction]]
   * <console>:15: error: could not find implicit value for parameter e: Ordered[Chapter11.Fraction]
   *        implicitly[Ordered[Fraction]]
   *                  ^
   *
   * scala> implicitly[Ordering[Fraction]]
   * res2: Ordering[Chapter11.Fraction] = Chapter21$FractionOrdering@3af82646
   *
   * }}}
   * For the second case, `implicitly[Ordered[Fraction]]`, it didn't work, since there is no
   * appropriate implicit value defined. And our `implicit class RichFraction` is not suitable
   * since it requires an input parameter.
   */
  case class Delimiters(left: String, right: String)

  implicit val quoteDelimiters = Delimiters("<<", ">>")

  class FractionOrdering extends Ordering[Fraction] {

    override def compare(x: Fraction, y: Fraction): Int = x.compare(y)
  }

  implicit val fractionOrdering = new FractionOrdering

  /**
   * Task 9:
   *
   * Look up the `=:=` object in `Predef.scala`. Explain how it works.
   *
   * Solution:
   *
   * It is defined like this:
   * {{{
   *   sealed abstract class =:=[From, To] extends (From => To) with Serializable
   *   private[this] final val singleton_=:= = new =:=[Any,Any] { def apply(x: Any): Any = x }
   *   object =:= {
   *     implicit def tpEquals[A]: A =:= A = singleton_=:=.asInstanceOf[A =:= A]
   *   }
   * }}}
   * So, `=:=` is a `Function` class with one argument and with one singleton instance and
   * implicit conversion method `tpEquals`, defined in its companion object.
   * When it is used as implicit evidence parameter:
   * {{{
   * def someMethod[A, B](obj: A)(implicit ev: A =:= B): Unit = {
   *   //can call methods, defined in B
   *   obj.methodInB()
   * }
   * }}}
   * compiler sees that implicit argument is a function with one parameter, so it calls it
   * {{{
   * ev(obj).methodInB()
   * }}}
   * to convert instance from one type `A` to the given type (`B` in this case).
   * In this way compiler can check/prove that instance confirms to the given constraint
   * (`=:=` equal types, in this case).
   */

  /**
   * Task 10:
   *
   * The result of `"abc".map(_.toUpper)` is a `String`, but the result of `"abc".map(_.toInt)`
   * is a Vector. Find out why.
   *
   * Solution:
   *
   * Since `CanBuildFrom` factory trait for `String` is defined in the `WrappedString` companion
   * object like this:
   * {{{
   * implicit def canBuildFrom: CanBuildFrom[WrappedString, Char, WrappedString]
   * }}}
   * it can build result `String` collection only for `Char` elements.
   * For other element types, like `Int`, as defined in the `IndexedSeq` companion object
   * {{{
   * def newBuilder[A]: Builder[A, IndexedSeq[A]] = Vector.newBuilder[A]
   * }}}
   * `Vector` is the current default implementation.
   */
  def stringMapTest(): Unit = {
    val str: String = "abc".map(_.toUpper)
    val seq: IndexedSeq[Int] = "abc".map(_.toInt)
  }
}
