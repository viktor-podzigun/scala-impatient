

object Chapter17 {

  /**
   * Task 1:
   *
   * Define an immutable `class Pair[T, S]` with a method `swap` that returns a new pair with
   * the components swapped.
   */
  case class Pair[T, S](first: T, second: S) {

    def swap(): Pair[S, T] = Pair(second, first)
  }

  /**
   * Task 2:
   *
   * Define a mutable `class Pair[T]` with a method `swap` that swaps the components of the pair.
   */
  class MutablePair[T](var first: T, var second: T) {

    def swap(): Unit = {
      val tmp = first
      first = second
      second = tmp
    }
  }
}
