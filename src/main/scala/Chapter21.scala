

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
}
