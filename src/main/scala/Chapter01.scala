

object Chapter01 {

  /**
   * Task 1:
   *
   * In the Scala REPL, type `3.` followed by the Tab key. What methods can be applied?
   *
   * Solution:
   *
   * On my machine, when typing `3.` and pressing the Tab key for the first time,
   * these methods are available:
   * {{{
   * %   &   *   +   -   /   >   >=   >>   >>>   ^   asInstanceOf   isInstanceOf
   * toByte   toChar   toDouble   toFloat   toInt   toLong   toShort   toString
   * unary_+   unary_-   unary_~   |
   * }}}
   * And when pressing the Tab key the second time:
   * {{{
   * !=   %   *   -   <    <=       ==   >=   >>>   asInstanceOf   getClass   isInstanceOf
   * toChar     toFloat   toLong    toString   unary_-   |
   * ##   &   +   /   <<   <init>   >    >>   ^     equals         hashCode
   * toByte         toDouble   toInt     toShort   unary_+    unary_~
   * }}}
   */

  /**
   * Task 2:
   *
   * In the Scala REPL, compute the square root of 3, and then square that value.
   * By how much does the result differ from 3? (Hint: The res variables are your friend.)
   *
   * Solution:
   *
   * {{{
   * scala> Math.sqrt(3)
   * res0: Double = 1.7320508075688772
   *
   * scala> res0 * res0
   * res1: Double = 2.9999999999999996
   *
   * scala> 3 - res1
   * res2: Double = 4.440892098500626E-16
   * }}}
   */

  /**
   * Task 3:
   *
   * Are the res variables `val` or `var` ?
   *
   * Solution:
   *
   * They are `val`:
   * {{{
   * scala> res2 = 1.0
   * <console>:13: error: reassignment to val
   *        res2 = 1.0
   *             ^
   * }}}
   */

  /**
   * Task 4:
   *
   * Scala lets you multiply a string with a number - try out `"crazy" * 3` in the REPL.
   * What does this operation do? Where can you find it in `Scaladoc`?
   *
   * Solution:
   *
   * This operation concatenates the given string to itself the given number of times:
   * {{{
   * scala> "crazy" * 3
   * res3: String = crazycrazycrazy
   * }}}
   * It can be found in `Scaladoc` by searching for `StringOps` class, it has the following
   * method:
   * {{{
   * def *(n: Int): String
   * Return the current string concatenated n times.
   * }}}
   */

  /**
   * Task 5:
   *
   * What does `10 max 2` mean? In which class is the `max` method defined?
   *
   * Solution:
   *
   * It compares two integer values and returns the maximum value.
   * The `max` method is defined in `RichInt` class.
   * The `Int` value `10` is first converted to a `RichInt` and then the `max` method is called
   * on that value.
   */

  /**
   * Task 6:
   *
   * Using `BigInt`, compute `2^^1024`.
   */
  def computeBigInt(): BigInt = BigInt(2).pow(1024)

  /**
   * Task 7:
   *
   * What do you need to `import` so that you can get a random prime as
   * `probablePrime(100, Random)`, without any qualifiers before `probablePrime` and `Random`?
   */
  def task7(): Unit = {
    import scala.BigInt._
    import scala.util._

    probablePrime(100, Random)
  }
}
