

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

  /**
   * Task 6:
   *
   * Modify the previous function to return the input at which the output is largest. For example,
   * `largestAt(x => 10 * x - x * x, 1 to 10)` should return `5`. Don't use a loop or recursion.
   */
  def largestAt(fun: (Int) => Int, inputs: Seq[Int]): Int = {
    val valueWithInput: (Int, Int) = inputs.map(i => (fun(i), i)).reduceLeft {(a, b) =>
      if (a._1 > b._1) a
      else b
    }

    valueWithInput._2
  }

  /**
   * Task 7:
   *
   * It's easy to get a sequence of pairs, for example
   * {{{
   *  val pairs = (1 to 10) zip (11 to 20)
   * }}}
   * Now suppose you want to do something with such a sequence - say, add up the values.
   * But you can't do
   * {{{
   *  pairs.map(_ + _)
   * }}}
   * The function `_ + _` takes two `Int` parameters, not an `(Int, Int)` pair. Write a function
   * `adjustToPair` that receives a function of type `(Int, Int) => Int` and returns the equivalent
   * function that operates on a pair. For example, `adjustToPair(_ * _)((6, 7))` is `42`.
   * Then use this function in conjunction with `map` to compute the sums of the elements in pairs.
   */
  def adjustToPair(fun: (Int, Int) => Int): ((Int, Int)) => Int = {
    (pair: (Int, Int)) => fun(pair._1, pair._2)
  }

// Or using currying:
//
//  def adjustToPair(fun: (Int, Int) => Int)(pair: (Int, Int)): Int = {
//    fun(pair._1, pair._2)
//  }

  def mapPairs(pairs: Seq[(Int, Int)], fun: (Int, Int) => Int): Seq[Int] = {
    pairs.map(adjustToPair(fun))
  }

  /**
   * Task 8:
   *
   * In Section 12.8, "Currying", on page 149, you saw the `corresponds` method used with two
   * arrays of strings. Make a call to corresponds that checks whether the elements in an
   * array of strings have the lengths given in an array of integers.
   */
  def correspondsLen(strings: Array[String], lengths: Array[Int]): Boolean = {
    strings.corresponds(lengths)(_.length == _)
  }

  /**
   * Task 9:
   *
   * Implement `corresponds` without currying. Then try the call from the preceding exercise.
   * What problem do you encounter?
   *
   * Solution:
   *
   * Without currying the compiler is not be able to infer the types for predicate function.
   * So, it should be called like this:
   * {{{
   *  corresponds2(Array("a"), Array(1), (a: String, b: Int) => a.length == b)
   * }}}
   */
  def corresponds2[A, B](ax: Array[A], bx: Array[B], predicate: (A, B) => Boolean): Boolean = {
    ax.corresponds(bx)(predicate)
  }

  /**
   * Task 10:
   *
   * Implement an `unless` control abstraction that works just like `if`, but with an inverted
   * condition. Does the first parameter need to be a call-by-name parameter? Do you need currying?
   *
   * Solution:
   *
   * The first parameter not need to be a call-by-name parameter since it is evaluated only once.
   * Yes, we need currying, with it looks exactly like an `if` expression.
   */
  def unless(cond: Boolean)(block: => Unit): Unit = if (!cond) block
}
