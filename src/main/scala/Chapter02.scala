import scala.math.Numeric.IntIsIntegral

object Chapter02 {

  /**
   * Task 1:
   *
   * The `signum` of a number is `1` if the number is positive, `–1` if it is negative,
   * and `0` if it is zero. Write a function that computes this value.
   */
  def signum(n: Int): Int = {
    if (n < 0) -1
    else if (n > 0) 1
    else 0
  }

  /**
   * Task 2:
   *
   * What is the value of an empty block expression `{}`? What is its type?
   *
   * Solution:
   *
   * The value of an empty block expression `{}` is `()` and it's type is `scala.Unit`.
   * Which is equivalent to `java.lang.Void`.
   */
  def task2(): Unit = {}

  /**
   * Task 3:
   *
   * Come up with one situation where the assignment `x = y = 1` is valid in Scala.
   * (Hint: Pick a suitable type for `x`.)
   */
  def task3(): Unit = {
    var y: Int = 0
    var x: Unit = ()
    x = y = 1
  }

  /**
   * Task 4:
   *
   * Write a `Scala` equivalent for the `Java` loop
   * {{{
   * for (int i = 10; i >= 0; i--) System.out.println(i);
   * }}}
   */
  def task4(): Unit = for (i <- 10 to (0, -1)) println(i)

  /**
   * Task 5:
   *
   * Write a procedure `countdown(n: Int)` that prints the numbers from n to 0.
   */
  def countdown(n: Int): Unit = for (i <- n to (0, -1)) println(i)

  /**
   * Task 6:
   *
   * Write a `for` loop for computing the product of the Unicode codes of all letters
   * in a string. For example, the product of the characters in "Hello" is 825152896.
   */
  def productLoop(s: String): Int = {
    var result = 1
    for (c <- s) result *= c.toInt

    result
  }

  /**
   * Task 7:
   *
   * Solve the preceding exercise without writing a loop.
   * (Hint: Look at the `StringOps` Scaladoc.)
   */
  def productNoLoop(s: String): Int = s.map(_.toInt).product

  /**
   * Task 8:
   *
   * Write a function `product(s : String)` that computes the product, as described
   * in the preceding exercises.
   */
  def product(s : String): Int = productLoop(s)

  /**
   * Task 9:
   *
   * Make the function of the preceding exercise a recursive function.
   */
  def productRecursive(s : String): Int = {
    if (s.isEmpty) {
      return 1
    }

    s.head.toInt * productLoop(s.tail)
  }

  /**
   * Task 10:
   *
   * Write a function that computes `x^^n`, where `n` is an integer. Use the following
   * recursive definition:
   * {{{
   * x^n = y^2 if n is even and positive, where y = x^(n / 2).
   * x^n = x * x^(n – 1) if n is odd and positive.
   * x^0 = 1.
   * x^n = 1 / (x^–n) if n is negative.
   * }}}
   * Don't use a `return` statement.
   */
  def pow(x: Int, n: Int): Double = {
    if (n == 0) 1
    else if (n < 0) 1 / pow(x, -n)
    else if (n % 2 != 0) x * pow(x, n - 1)
    else {
      val y = pow(x, n / 2)
      y * y
    }
  }
}
