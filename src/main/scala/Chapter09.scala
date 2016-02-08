import java.io.{File, PrintWriter}

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Chapter09 {

  /**
   * Task 1:
   *
   * Write a Scala code snippet that reverses the lines in a file
   * (making the last line the first one, and so on).
   */
  def reverseLines(file: File): Unit = {
    val source: BufferedSource = Source.fromFile(file)
    val lines = try {
      source.getLines().toBuffer.reverse
    }
    finally {
      source.close()
    }

    val writer = new PrintWriter(file)
    try {
      lines.foreach(writer.println)
    }
    finally {
      writer.close()
    }
  }

  /**
   * Task 2:
   *
   * Write a Scala program that reads a file with tabs, replaces each tab with spaces
   * so that tab stops are at n-column boundaries, and writes the result to the same file.
   */
  def replaceTabs(file: File, charsPerColumn: Int = 4): Unit = {
    var chars = 0
    val buff = new ArrayBuffer[Char]
    val source: BufferedSource = Source.fromFile(file)
    try for (c <- source) c match {
      case '\t' => for (_ <- 0 until (charsPerColumn - chars % charsPerColumn)) buff += ' '
        chars = 0
      case '\n' => buff += c
        chars = 0
      case _ => buff += c
        chars += 1
    }
    finally {
      source.close()
    }

    val writer = new PrintWriter(file)
    try {
      buff.foreach(writer.print)
    }
    finally {
      writer.close()
    }
  }

  /**
   * Task 3:
   *
   * Write a Scala code snippet that reads a file and prints all words with more than
   * 12 characters to the console. Extra credit if you can do this in a single line.
   */
  def printLongWords(file: String, maxWordLength: Int = 12): Unit = {
    Source.fromFile(file).mkString.split("\\s+").filter(_.length > maxWordLength).foreach(println)
  }

  /**
   * Task 4:
   *
   * Write a Scala program that reads a text file containing only floating-point numbers.
   * Print the sum, average, maximum, and minimum of the numbers in the file.
   */
  def printNumbersStat(file: String): Unit = {
    var count: Int = 0
    var sum: Double = 0.0
    var min: Double = Double.MaxValue
    var max: Double = Double.MinValue
    for (token <- Source.fromFile(file).mkString.split("\\s+"); num = token.toDouble) {
      count += 1
      sum += num
      if (num < min) min = num
      if (num > max) max = num
    }

    printf("sum:     %.3f\n", sum)
    printf("average: %.3f\n", sum / count)
    printf("minimum: %.3f\n", min)
    printf("maximum: %.3f\n", max)
  }
}

object PrintLongWordsApp extends App {
  if (args.length < 1) {
    sys.error("Expect file name as first argument")
    System.exit(1)
  }

  Chapter09.printLongWords(args(0))
}

object PrintNumbersStatApp extends App {
  if (args.length < 1) {
    sys.error("Expect file name as first argument")
    System.exit(1)
  }

  Chapter09.printNumbersStat(args(0))
}
