import java.util
import java.util.Scanner

import scala.collection.JavaConversions.{mapAsScalaMap, propertiesAsScalaMap}
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
   * <blockquote><code>
   *     val in = new java.util.Scanner(new java.io.File("myfile.txt")) <br/>
   *     while (in.hasNext()) process in.next()
   * </code></blockquote>
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

  /**
   * Task 4:
   *   Repeat the preceding exercise with a sorted map,
   *   so that the words are printed in sorted order.
   */
  def countWordsSortedMap(): Map[String, Int] = {
    var words = SortedMap[String, Int]()

    processWords(w => words += w -> (words.getOrElse(w, 0) + 1))
    words
  }

  /**
   * Task 5:
   *   Repeat the preceding exercise with a java.util.TreeMap
   *   that you adapt to the Scala API.
   */
  def countWordsTreeMap(): mutable.Map[String, Int] = {
    val words = new util.TreeMap[String, Int]()

    processWords(w => words(w) = words.getOrElse(w, 0) + 1)
    words
  }

  /**
   * Task 6:
   *   Define a linked hash map that maps "Monday" to java.util.Calendar.MONDAY,
   *   and similarly for the other weekdays. Demonstrate that the elements
   *   are visited in insertion order.
   */
  def weekdaysLinkedHashMap(): mutable.Map[String, Int] = {
    val weekdays = mutable.LinkedHashMap[String, Int]("Monday" -> util.Calendar.MONDAY)
    weekdays += ("Tuesday" -> util.Calendar.TUESDAY,
      "Wednesday" -> util.Calendar.WEDNESDAY,
      "Thursday" -> util.Calendar.THURSDAY,
      "Friday" -> util.Calendar.FRIDAY,
      "Saturday" -> util.Calendar.SATURDAY,
      "Sunday" -> util.Calendar.SUNDAY)

    weekdays
  }

  /**
   * Task 7:
   *   Print a table of all Java properties, like this:
   * <blockquote><code>
   *   java.runtime.name     | Java(TM) SE Runtime Environment <br/>
   *   sun.boot.library.path | /home/apps/jdk1.6.0_21/jre/lib/i386 <br/>
   *   java.vm.version       | 17.0-b16 <br/>
   *   java.vm.vendor        | Sun Microsystems Inc. <br/>
   *   java.vendor.url       | http://java.sun.com/ <br/>
   *   path.separator        | : <br/>
   *   java.vm.name          | Java HotSpot(TM) Server VM <br/>
   * </code></blockquote>
   *   You need to find the length of the longest key before you can print the table.
   */
  def formatJavaProperties(): List[String] = {
    val props: collection.Map[String, String] = System.getProperties
    val maxKeyLen = props.foldLeft(0)((maxLen, entry) => {
      if (maxLen < entry._1.length) entry._1.length
      else maxLen
    })

    val result = ListBuffer[String]()
    for ((key, value) <- props) {
      result += key.padTo(maxKeyLen, ' ') + " | " + value
    }

    result.toList
  }

  /**
   * Task 8:
   *   Write a function minmax(values: Array[Int]) that returns a pair containing
   *   the smallest and largest values in the array.
   */
  def minmax(values: Array[Int]): (Int, Int) = (values.min, values.max)

  /**
   * Task 9:
   *   Write a function lteqgt(values: Array[Int], v: Int) that returns a triple containing
   *   the counts of values less than v , equal to v , and greater than v.
   */
  def lteqgt(values: Array[Int], v: Int): (Int, Int, Int) = {
//    val count = values.count _
//
//    (count(_ < v), count(_ == v), count(_ > v))
    var lt, eq, gt = 0

    for (e <- values) {
      if (e < v) lt += 1
      else if (e == v) eq += 1
      else gt += 1
    }

    (lt, eq, gt)
  }

  def main(args: Array[String]) {
    // task 2
    println(countWordsMutableMap().mkString("\n"))

    // task 7
    println(formatJavaProperties().mkString("\n"))
  }
}
