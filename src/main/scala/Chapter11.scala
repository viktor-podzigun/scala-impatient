import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Chapter11 {

  /**
   * Task 1:
   *
   * According to the precedence rules, how are `3 + 4 -> 5` and `3 -> 4 + 5` evaluated?
   *
   * Solution:
   *
   * Since `+` and `-` has the same precedence, then:
   * `3 + 4 -> 5` evaluated as `(3 + 4) -> 5` and
   * `3 -> 4 + 5` evaluated as `(3 -> 4) + 5`
   */

  /**
   * Task 2:
   *
   * The `BigInt` class has a `pow` method, not an operator.
   * Why didn't the Scala library designers choose `**` (as in Fortran) or `^` (as in Pascal)
   * for a power operator?
   *
   * Solution:
   *
   * First, the precedence is determined by the first character, the `**` operator would has
   * the same precedence as `*`, `/` operators, but `pow` operation should have higher precedence.
   * Second, the `^` operator is already exists for `BigInt` class, which is
   * "Bitwise exclusive-or of BigInts".
   */

  /**
   * Task 3:
   *
   * Implement the `Fraction` class with operations `+`, `-`, `*`, `/`. Normalize fractions,
   * for example turning `15/-6` into `-5/2`. Divide by the greatest common divisor, like this:
   * {{{
   *  class Fraction(n: Int, d: Int) {
   *    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
   *    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
   *    override def toString = num + "/" + den
   *    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
   *    def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)
   *    ...
   *  }
   * }}}
   */
  class Fraction private(n: Int, d: Int) {

    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)

    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)

    private def sign(a: Int) =
      if (a > 0) 1
      else if (a < 0) -1
      else 0

    @tailrec
    private def gcd(a: Int, b: Int): Int =
      if (b == 0) Math.abs(a)
      else gcd(b, a % b)

    def +(other: Fraction): Fraction = sumOp(other, _ + _)

    def -(other: Fraction): Fraction = sumOp(other, _ - _)

    def *(other: Fraction): Fraction = Fraction(num * other.num, den * other.den)

    def /(other: Fraction): Fraction = Fraction(num * other.den, den * other.num)

    private def sumOp(other: Fraction, op: (Int, Int) => Int): Fraction = {
      val div = gcd(den, other.den)
      Fraction(op(num * (div / den), other.num * (div / other.den)), div)
    }

    override def toString = num + "/" + den
  }

  object Fraction {
    def apply(n: Int, d: Int) = new Fraction(n, d)
  }

  /**
   * Task 4:
   *
   * Implement a class `Money` with fields for dollars and cents. Supply `+`, `-` operators
   * as well as comparison operators `==` and `<`. For example,
   * {{{
   *  Money(1, 75) + Money(0, 50) == Money(2, 25)
   *  }}}
   * should be true. Should you also supply `*` and `/` operators? Why or why not?
   *
   * Solution:
   *
   * Operators `*` and `/` for `Money` class doesn't make sense, since usually money multiplied
   * or divided by concrete number, not another amount of money.
   */
  class Money private(d: Int, c: Int) {

    if (d < 0) throw new IllegalArgumentException("d: " + d)
    if (c < 0) throw new IllegalArgumentException("c: " + c)

    val dollars: Int = if (c > 99) d + (c / 100) else d
    val cents: Int = if (c > 99) c % 100 else c

    def +(other: Money): Money = Money(dollars + other.dollars, cents + other.cents)

    def -(other: Money): Money = {
      val d: Int = dollars - other.dollars
      val c: Int = cents - other.cents
      if (c < 0) Money(d - 1, c + 100)
      else Money(d, c)
    }

    def ==(other: Money): Boolean = dollars == other.dollars && cents == other.cents

    def <(other: Money): Boolean = (dollars * 100) + cents < (other.dollars * 100) + other.cents

    override def toString = "%d.%02d".format(dollars, + cents)
  }

  object Money {
    def apply(d: Int, c: Int) = new Money(d, c)
  }

  /**
   * Task 5:
   *
   * Provide operators that construct an HTML table. For example,
   * {{{
   *  Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"
   * }}}
   * should produce
   * {{{
   *  <table><tr><td>Java</td><td>Scala</td</tr><tr><td>Gosling...
   * }}}
   */
  class Table private {

    private val table = new ListBuffer[ListBuffer[String]]

    def |(cell: String): Table = {
      if (table.isEmpty) {
        table += new ListBuffer[String]
      }

      table.last += cell
      this
    }

    def ||(cell: String): Table = {
      table += new ListBuffer[String]
      table.last += cell
      this
    }

    def toHtml: String = {
      val sb = new StringBuilder("<table>")
      for (row <- table) {
        sb ++= "<tr>"
        for (cell <- row) {
          sb ++= "<td>"
          sb ++= cell
          sb ++= "</td>"
        }

        sb ++= "</tr>"
      }

      sb ++= "</table>"
      sb.toString()
    }

    override def toString = table.mkString("\n")
  }

  object Table {
    def apply() = new Table
  }
}
