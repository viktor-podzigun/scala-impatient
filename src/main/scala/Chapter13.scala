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
}
