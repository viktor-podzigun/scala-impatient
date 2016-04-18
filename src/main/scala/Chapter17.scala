

/**
 * Task 1:
 *
 * Define an immutable `class Pair[T, S]` with a method `swap` that returns a new pair with
 * the components swapped.
 */
package task1701 {

class Pair[T, S](val first: T, val second: S) {

  def swap(): Pair[S, T] = new Pair(second, first)
}

}

/**
 * Task 2:
 *
 * Define a mutable `class Pair[T]` with a method `swap` that swaps the components of the pair.
 */
package task1702 {

class Pair[T](var first: T, var second: T) {

  def swap(): Unit = {
    val tmp = first
    first = second
    second = tmp
  }
}

}

/**
 * Task 3:
 *
 * Given a `class Pair[T, S]`, write a generic method `swap` that takes a pair as its argument
 * and returns a new pair with the components swapped.
 */
object Chapter17Task03 {

  import task1701._

  def swap[T, S](pair: Pair[T, S]): Pair[S, T] = new Pair(pair.second, pair.first)

}

/**
 * Task 4:
 *
 * Why don't we need a lower bound for the `replaceFirst` method in Section 17.3,
 * "Bounds for Type Variables”, on page 232 if we want to replace the first component of
 * a `Pair[Person]` with a `Student`?
 *
 * Solution:
 *
 * We don't need a lower bound because we replacing with a sub-class, which is OK, since
 * the result type is still `Pair[Person]`.
 */
package task1704 {

class Person(val name: String)
class Student(name: String) extends Person(name)

class Pair[T](val first: T, val second: T) {

  def replaceFirst(newFirst: T): Pair[T] = new Pair(newFirst, second)
}

}

/**
 * Task 5:
 *
 * Why does `RichInt` implement `Comparable[Int]` and not `Comparable[RichInt]`?
 *
 * Solution:
 *
 * To be able to use view bounds, like
 * {{{
 *   class Pair[T <% Comparable[T]](val first: T, val second: T)
 * }}}
 * which than can be used with `Int` types, like
 * {{{
 *   new Pair(1, 2)
 * }}}
 * we need to have implicit conversion from `T` to `Comparable[T]`. `RichInt` class
 * implements `Comparable[Int]` and there is implicit conversion from `Int` to `RichInt`.
 * So, we don't use `RichInt` class directly.
 */

/**
 * Task 6:
 *
 * Write a generic method `middle` that returns the middle element from any `Iterable[T]`.
 * For example, `middle("World")` is 'r'.
 */
object Chapter17Task06 {

  //def middle[T](xs: Iterable[T]): Option[T] = {
  //def middle[A, C](xs: C)(implicit ev: C <:< Iterable[A]): Option[A] = {
  //def middle[A, C <% Iterable[A]](xs: C): Option[A] = {
  def middle[A, C](xs: C)(implicit ev: C => Iterable[A]): Option[A] = {
    val size = xs.size
    if (size % 2 == 0) {
      return None
    }

    var distance = size / 2
    xs.find { _ =>
      val found = if (distance == 0) true else false
      distance -= 1
      found
    }
  }
}

/**
 * Task 7:
 *
 * Look through the methods of the `Iterable[+A]` trait. Which methods use the type parameter `A`?
 * Why is it in a covariant position in these methods?
 *
 * Solution:
 *
 * Parameter `A` used in the following methods, for example: `head`, `last`, `min`, `max`.
 * Its used in a covariant position since its defined with the covariant variance annotation (+A).
 */

/**
 * Task 8:
 *
 * In Section 17.10, "Co- and Contravariant Positions", on page 238, the `replaceFirst` method
 * has a type bound. Why can't you define an equivalent method on a mutable `Pair[T]`?
 * {{{
 *   def replaceFirst[R >: T](newFirst: R) { first = newFirst } // Error
 * }}}
 *
 * Solution:
 *
 * It is an error because the used type bound (`R >: T`) allows passing supper type instances.
 * But we can change the type bound definition to allow passing sub type instances:
 * {{{
 *   def replaceFirst[R <: T](newFirst: R) { first = newFirst }
 * }}}
 */

/**
 * Task 9:
 *
 * It may seem strange to restrict method parameters in an immutable `class Pair[+T]`. However,
 * suppose you could define
 * {{{
 *   def replaceFirst(newFirst: T)
 * }}}
 * in a `Pair[+T]`. The problem is that this method can be overridden in an unsound way.
 * Construct an example of the problem. Define a subclass `NastyDoublePair` of `Pair[Double]`
 * that overrides `replaceFirst` so that it makes a pair with the square root of `newFirst`.
 * Then construct the call `replaceFirst("Hello")` on a `Pair[Any]` that is actually
 * a `NastyDoublePair`.
 */
object Chapter17Task09 {

  class Pair[+T](val first: T, val second: T) {

    //def replaceFirst(newFirst: T): Pair[T] = new Pair(newFirst, second)

    def replaceFirst[R >: T](newFirst: R): Pair[R] = new Pair(newFirst, second)
  }

  class NastyDoublePair(first: Double, second: Double) extends Pair[Double](first, second) {

    //override def replaceFirst[R >: Double](newFirst: R) = new Pair(math.sqrt(newFirst), second)

    override def replaceFirst[R >: Double](newFirst: R) = new Pair(math.sqrt(first), second)
  }

  def check(pair: Pair[Any]): Pair[Any] = pair.replaceFirst("Hello")
}

/**
 * Task 10:
 *
 * Given a mutable `Pair[S, T]` class, use a type constraint to define a `swap` method that can
 * be called if the type parameters are the same.
 */
package task1710 {

class Pair[S, T](var first: S, var second: T) {

  def swap(implicit ev: T =:= S): Unit = {
    val tmp = first.asInstanceOf[T]
    first = second
    second = tmp
  }
}

}
