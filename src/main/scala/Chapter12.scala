

object Chapter12 {

  /**
   * Task 1:
   *
   * Write a function `values(fun: (Int) => Int, low: Int, high: Int)` that yields a collection
   * of function inputs and outputs in a given range. For example, `values(x => x * x, -5, 5)`
   * should produce a collection of pairs `(-5, 25)`, `(-4, 16)`, `(-3, 9)`, ..., `(5, 25)`.
   */
  def values(fun: (Int) => Int, low: Int, high: Int): Seq[(Int, Int)] = {
    for (i <- low to high) yield (i, fun(i))
  }

  /**
   * Task 2:
   *
   * How do you get the largest element of an array with `reduceLeft`?
   */
  def largestElement(arr: Array[Int]): Int = arr.reduceLeft((a, b) => if (a > b) a else b)

  /**
   * Task 3:
   *
   * Implement the `factorial` function using `to` and `reduceLeft`, without a loop or recursion.
   */
  def factorial(n: Int): Int = if (n <= 0) 1 else (1 to n).reduceLeft(_ * _)

  /**
   * Task 4:
   *
   * The previous implementation needed a special case when `n < 1`. Show how you can avoid this
   * with `foldLeft`. (Look at the Scaladoc for `foldLeft`. It’s like `reduceLeft`, except that
   * the first value in the chain of combined values is supplied in the call.)
   */
  def factorial2(n: Int): Int = (1 to n).foldLeft(1)(_ * _)

  /**
   * Task 5:
   *
   * Write a function `largest(fun: (Int) => Int, inputs: Seq[Int])` that yields the largest
   * value of a function within a given sequence of inputs. For example,
   * `largest(x => 10 * x - x * x, 1 to 10)` should return `25`. Don't use a loop or recursion.
   */
  def largest(fun: (Int) => Int, inputs: Seq[Int]): Int = {
    inputs.map(fun(_)).reduceLeft((a, b) => if (a > b) a else b)
  }
}
