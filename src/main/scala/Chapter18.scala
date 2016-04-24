import scala.beans.BeanProperty

object Chapter18 {

  /**
   * Task 1:
   *
   * Implement a `Bug` class modeling a bug that moves along a horizontal line.
   * The `move` method moves in the current direction,
   * the `turn` method makes the bug turn around,
   * and the `show` method prints the current position.
   * Make these methods chainable. For example,
   * {{{
   *   bugsy.move(4).show().move(6).show().turn().move(5).show()
   * }}}
   * should display `4 10 5`.
   */
  class Bug {

    private var position: Int = 0
    private var turnedAround: Boolean = false

    def move(steps: Int): this.type = {
      if (turnedAround) position -= steps
      else position += steps
      this
    }

    def turn(): this.type = {
      turnedAround = !turnedAround
      this
    }

    def show(): this.type = {
      print(" ")
      print(position)
      this
    }
  }

  /**
   * Task 2:
   *
   * Provide a fluent interface for the `Bug` class of the preceding exercise, so that one can
   * write
   * {{{
   *   bugsy move 4 and show and then move 6 and show turn around move 5 and show
   * }}}
   */
  trait FluentBug { this: Bug =>

    def and(obj: Show.type): this.type = this.show()

    def and(obj: Then.type): this.type = this

    def turn(obj: Around.type): this.type = this.turn()
  }

  object Show
  object Then
  object Around

  val show = Show
  val then = Then
  val around = Around

  /**
   * Task 3:
   *
   * Complete the fluent interface in Section 18.1, "Singleton Types", on page 246
   * so that one can call
   * {{{
   *   book set Title to "Scala for the Impatient" set Author to "Cay Horstmann"
   * }}}
   */
  trait FluentDocument { this: Document =>

    private var useNextArgsAs: Option[Any] = None

    def set(obj: Title.type): this.type = setNextArgsAs(obj)
    def set(obj: Author.type): this.type = setNextArgsAs(obj)

    def to(arg: String): this.type = {
      for (obj <- useNextArgsAs) obj match {
        case Title => setTitle(arg)
        case Author => setAuthor(arg)
      }

      this
    }

    private def setNextArgsAs(obj: Any): this.type = {
      useNextArgsAs = Some(obj)
      this
    }
  }

  object Title
  object Author

  class Document {

    @BeanProperty var title: String = null
    @BeanProperty var author: String = null
  }

  class Book extends Document with FluentDocument {

    def addChapter(chapter: String): this.type = this
  }

  /**
   * Task 4:
   *
   * Implement the `equals` method for the `Member` class that is nested inside the `Network`
   * class in Section 18.2, "Type Projections", on page 247. For two members to be equal,
   * they need to be in the same network.
   */
  class Network {

    class Member {

      override def equals(that: Any): Boolean = that match {
        case _: Member => true
        case _ => false
      }
    }
  }

  /**
   * Task 5:
   *
   * Consider the type alias
   * {{{
   *   type NetworkMember = n.Member forSome { val n: Network }
   * }}}
   * and the function
   * {{{
   *   def process(m1: NetworkMember, m2: NetworkMember) = (m1, m2)
   * }}}
   * How does this differ from the `process` function in Section 18.8, "Existential Types",
   * on page 252?
   *
   * Solution:
   *
   * The difference is that `NetworkMember` defines type alias for `Member` from any `Network`,
   * which is the same as defining `process` function like this:
   * {{{
   *   def process[M >: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)
   * }}}
   * While the `process` function, from Section 18.8, accepts only members from the same network,
   * and it is defined like this:
   * {{{
   *   def process[M <: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)
   * }}}
   */
  import scala.language.existentials

  type NetworkMember = n.Member forSome { val n: Network }

  def process(m1: NetworkMember, m2: NetworkMember) = (m1, m2)

  def processAny[M >: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)

  def processSame[M <: n.Member forSome { val n: Network }](m1: M, m2: M) = (m1, m2)

  /**
   * Task 6:
   *
   * The `Either` type in the Scala library can be used for algorithms that return either a result
   * or some failure information. Write a function that takes two parameters:
   * a sorted array of integers and an integer value.
   * Return either the index of the value in the array or the index of the element that is closest
   * to the value. Use an infix type as the return type.
   */
  def findClosestValueIndex(sortedArr: Array[Int], value: Int): Int Either Int = {
    var i = 0
    while (i < sortedArr.length) {
      val elem = sortedArr(i)
      if (elem == value) return Left(i)
      else if (elem > value) return Right(i)
      i += 1
    }

    // return index if the last element
    Right(i - 1)
  }

  /**
   * Task 7:
   *
   * Implement a method that receives an object of any class that has a method
   * {{{
   *   def close(): Unit
   * }}}
   * together with a function that processes that object. Call the function and invoke the
   * `close` method upon completion, or when any exception occurs.
   */
  def processAndClose[T <: { def close(): Unit }, R](obj: T)(f: T => R): R = {
    import scala.language.reflectiveCalls

    try {
      f(obj)
    }
    finally {
      obj.close()
    }
  }

  /**
   * Task 8:
   *
   * Write a function `printValues` with three parameters `f`, `from`, `to` that prints all values
   * of `f` with inputs from the given range. Here, `f` should be any object with an `apply` method
   * that consumes and yields an `Int`. For example,
   * {{{
   *   printValues((x: Int) => x * x, 3, 6) // Prints 9 16 25 36
   *   printValues(Array(1, 1, 2, 3, 5, 8, 13, 21, 34, 55), 3, 6) // Prints 3 5 8 13
   * }}}
   */
  def printValues(f: { def apply(i: Int): Int }, from: Int, to: Int): Unit = {
    var i = from
    while (i <= to) {
      print(" ")
      print(f(i))
      i += 1
    }
  }

  /**
   * Task 9:
   *
   * Consider this class that models a physical dimension:
   * {{{
   *   abstract class Dim[T](val value: Double, val name: String) {
   *     protected def create(v: Double): T
   *     def +(other: Dim[T]) = create(value + other.value)
   *     override def toString() = value + " " + name
   *   }
   * }}}
   * Here is a concrete subclass:
   * {{{
   *   class Seconds(v: Double) extends Dim[Seconds](v, "s") {
   *     override def create(v: Double) = new Seconds(v)
   *   }
   * }}}
   * But now a knucklehead could define
   * {{{
   *   class Meters(v: Double) extends Dim[Seconds](v, "m") {
   *     override def create(v: Double) = new Seconds(v)
   *   }
   * }}}
   * allowing meters and seconds to be added. Use a self type to prevent that.
   */
  abstract class Dim[T](val value: Double, val name: String) { this: T =>

    protected def create(v: Double): T

    def +(other: Dim[T]) = create(value + other.value)

    override def toString = value + " " + name
  }

  class Seconds(v: Double) extends Dim[Seconds](v, "s") {
    override def create(v: Double) = new Seconds(v)
  }

  class Meters(v: Double) extends Dim[Meters](v, "m") {
    override def create(v: Double) = new Meters(v)
  }
}

/**
 * Task 10:
 *
 * Self types can usually be replaced with traits that extend classes, but there can be
 * situations where using self types changes the initialization and override orders.
 * Construct such an example.
 */
package task1810 {

  class A(val name: String) {

    override def toString: String = name
  }

  trait Named { self: A =>

    override val name: String = "Named: " + self.name

    override def toString: String = "from Named: " + super.toString
  }

  object Task1810App extends App {

    val obj = new A("obj") with Named

    println(obj) // Prints from Named: Named: null
  }
}
