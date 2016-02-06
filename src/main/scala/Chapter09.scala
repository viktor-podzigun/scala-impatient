import java.io.{File, PrintWriter}

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
}
