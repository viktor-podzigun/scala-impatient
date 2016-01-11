import java.util.Scanner

import scala.collection.mutable

object Chapter04 {

  /**
   * Task 1:
   *   Set up a map of prices for a number of gizmos that you covet.
   *   Then produce a second map with the same keys and the prices at a 10 percent discount.
   */
  def gizmosWithReducedPrice(): Map[String, Int] = {
    val gizmos = Map("iPhone" -> 600,
      "iPad" -> 500,
      "MacBook Pro" -> 2000,
      "ScalaDays 2016 Berlin" -> 750)

    gizmos.mapValues(price => price - (price / 10))
  }

  /**
   * Task 2:
   *   Write a program that reads words from a file. Use a mutable map to count
   *   how often each word appears. To read the words, simply use a java.util.Scanner:
   * <blockquote>
   *     val in = new java.util.Scanner(new java.io.File("myfile.txt")) <br/>
   *     while (in.hasNext()) process in.next()
   * </blockquote>
   */
  def countWordsMutableMap(): mutable.Map[String, Int] = {
    val words = new mutable.HashMap[String, Int]

    processWords(w => words(w) = words.getOrElse(w, 0) + 1)
    words
  }

  private def processWords(process: String => Unit): Unit = {
    val in = new Scanner(getClass.getResourceAsStream("/myfile.txt"))
    try {
      while (in.hasNext) {
        process(in.next())
      }
    } finally {
      in.close()
    }
  }

  /**
   * Task 3:
   *   Repeat the preceding exercise with an immutable map.
   */
  def countWordsImmutableMap(): Map[String, Int] = {
    var words = Map[String, Int]()

    processWords(w => words += w -> (words.getOrElse(w, 0) + 1))
    words
  }

  def main(args: Array[String]) {
    // task 2
    println(countWordsMutableMap().mkString("\n"))
  }
}
