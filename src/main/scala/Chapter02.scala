

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
}
