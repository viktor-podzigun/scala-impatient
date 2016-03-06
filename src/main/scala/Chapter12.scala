

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
}
