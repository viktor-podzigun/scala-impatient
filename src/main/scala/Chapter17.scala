

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
}
