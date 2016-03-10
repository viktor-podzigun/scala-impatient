import scala.collection.mutable

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
}
