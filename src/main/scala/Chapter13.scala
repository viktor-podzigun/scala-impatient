import scala.collection.{concurrent, mutable}
import scala.io.Source

object Chapter13 {

  /**
   * Task 1:
   *
   * Write a function that, given a string, produces a map of the indexes of all characters.
   * For example, `indexes("Mississippi")` should return a map associating
   * 'M' with the set {0},
   * ‘i’ with the set {1, 4, 7, 10}, and so on.
   * Use a mutable map of characters to mutable sets. How can you ensure that the set is sorted?
   *
   * Solution:
   *
   * We have to use `LinkedHashSet` to maintain the indices order in set.
   */
  def indexes(s: String): mutable.Map[Char, mutable.Set[Int]] = {
    val map = new mutable.HashMap[Char, mutable.Set[Int]]
    for (i <- 0 until s.length) {
      map.getOrElseUpdate(s(i), new mutable.LinkedHashSet[Int]) += i
    }

    map
  }

  /**
   * Task 2:
   *
   * Repeat the preceding exercise, using an immutable map of characters to lists.
   */
  def indexes2(s: String): Map[Char, List[Int]] = {
    var map = Map[Char, List[Int]]()
    for (i <- 0 until s.length) {
      val c = s(i)
      map = map.updated(c, map.getOrElse(c, Nil) :+ i)
    }

    map
  }

  /**
   * Task 3:
   *
   * Write a function that removes all zeroes from a linked list of integers.
   */
  def removeAllZeroes(list: mutable.LinkedList[Int]): mutable.LinkedList[Int] = {
    // remove elements at the beginning of the list
    var result = list
    while (result.nonEmpty && result.elem == 0) {
      result = result.next
    }

    // remove elements till the end of the list
    var prev, curr = result
    while (curr.nonEmpty) {
      if (curr.elem == 0) {
        prev.next = curr.next
        curr = prev
      }

      prev = curr
      curr = curr.next
    }

    result
  }

  /**
   * Task 4:
   *
   * Write a function that receives a collection of strings and a map from strings to integers.
   * Return a collection of integers that are values of the map corresponding to one of
   * the strings in the collection. For example, given
   * `Array("Tom", "Fred", "Harry")` and `Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)`,
   * return `Array(3, 5)`. Hint: Use flatMap to combine the Option values returned by get.
   */
  def mapToValues(coll: Traversable[String], map: Map[String, Int]): Traversable[Int] = {
    coll.flatMap(map.get)
  }

  /**
   * Task 5:
   *
   * Implement a function that works just like `mkString`, using `reduceLeft`.
   */
  def collToString[T](xs: Traversable[T]): String = {
    if (xs.isEmpty) ""
    // we have to specify Any type here, since its the closest common parent type for String and T
    else xs.reduceLeft((a: Any, b: T) => a + ", " + b).toString
  }

  /**
   * Task 6:
   *
   * Given a list of integers `lst`, what is
   * `(lst :\ List[Int]())(_ :: _)` ?
   * `(List[Int]() /: lst)(_ :+ _)` ?
   * How can you modify one of them to reverse the list?
   *
   * Solution:
   *
   * The first expression executes `foldRight` and prepends the elements to the resulting list.
   * The second expression executes `foldLeft` and appends the elements to the resulting list.
   * Both expressions produce new `List[Int]` with the same elements as in the original list,
   * and in the same order.
   * To reverse the list its better to modify the second expression to prepend the elements,
   * which is cheaper for lists comparing to append.
   */
  def reversList(lst: List[Int]): List[Int] = {
    //(lst :\ List[Int]())((el, res) => res :+ el)

    (List[Int]() /: lst)((res, el) => el :: res)
  }

  /**
   * Task 7:
   *
   * In Section 13.11, "Zipping", on page 171, the expression
   * {{{
   *  (prices zip quantities) map { p => p._1 * p._2 }
   * }}}
   * is a bit inelegant. We can't do
   * {{{
   *  (prices zip quantities) map { _ * _ }
   * }}}
   * because `_ * _` is a function with two arguments, and we need a function with one argument
   * that is a tuple. The `tupled` method of the `Function` object changes a function with
   * two arguments to one that take a tuple. Apply `tupled` to the multiplication function
   * so you can map it over the list of pairs.
   */
  def multiply(prices: Iterable[Int], quantities: Iterable[Int]): Iterable[Int] = {
    (prices zip quantities) map Function.tupled { _ * _ }
  }

  /**
   * Task 8:
   *
   * Write a function that turns an array of `Double` values into a two-dimensional array.
   * Pass the number of columns as a parameter. For example, with `Array(1, 2, 3, 4, 5, 6)`
   * and three columns, return `Array(Array(1, 2, 3), Array(4, 5, 6))`. Use the `grouped` method.
   */
  def twoDimensionalArray(arr: Array[Double], columns: Int): Array[Array[Double]] = {
    require(arr.length % columns == 0,
      "array length should be compatible with the number of columns")

    //val res: Array[Array[Double]] = Array.ofDim(arr.length / columns, columns)
    val res = new mutable.ArrayBuffer[Array[Double]](arr.length / columns)
    for (row <- arr.grouped(columns)) {
      res += row
    }

    res.toArray
  }

  /**
   * Task 9:
   *
   * Harry Hacker writes a program that accepts a sequence of file names on the command line.
   * For each, he starts a new thread that reads the file and updates a letter frequency map
   * declared as
   * {{{
   *  val frequencies = new scala.collection.mutable.HashMap[Char, Int] with
   *    scala.collection.mutable.SynchronizedMap[Char, Int]
   * }}}
   * When reading a letter `c`, he calls
   * {{{
   *  frequencies(c) = frequencies.getOrElse (c, 0) + 1
   * }}}
   * Why won't this work? Will it work if he used instead
   * {{{
   *  import scala.collection.JavaConversions.asScalaConcurrentMap
   *  val frequencies: scala.collection.mutable.ConcurrentMap[Char, Int] =
   *    new java.util.concurrent.ConcurrentHashMap[Char, Int]
   * }}}
   *
   * Solution:
   *
   * It won't work with SynchronizedMap since its not synchronize addition operation.
   * And its not enough using ConcurrentHashMap, we also need to perform threadsafe addition.
   * See the fixed code below.
   */
  def getLetterFrequencyMap(files: Iterable[String]): Map[Char, Int] = {
    import scala.collection.JavaConversions.mapAsScalaConcurrentMap

    //val frequencies = new mutable.HashMap[Char, Int] with mutable.SynchronizedMap[Char, Int]
    val frequencies: concurrent.Map[Char, Int] =
      new java.util.concurrent.ConcurrentHashMap[Char, Int]
    
    val threads = files.map(file =>
      new Thread(new Runnable() {
        override def run() = {
          val source = Source.fromInputStream(getClass.getResourceAsStream(file))
          try {
            for (c <- source) {
              //frequencies(c) = frequencies.getOrElse(c, 0) + 1
              var incremented = false
              while (!incremented) {
                val oldVal = frequencies.putIfAbsent(c, 0).getOrElse(0)
                incremented = frequencies.replace(c, oldVal, oldVal + 1)
              }
            }
          }
          finally {
            source.close()
          }
        }
      }))
    threads.foreach(_.start())
    threads.foreach(_.join())

    frequencies.toMap
  }
}
