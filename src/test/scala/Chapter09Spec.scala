import java.io.{PrintWriter, File}

import org.scalatest.{FlatSpec, Matchers}
import Chapter09._

import scala.io.Source

class Chapter09Spec extends FlatSpec with Matchers {

  "reverseLines" should "reverse lines in file" in {
    //given
    val file = File.createTempFile("reverseLines", "txt")
    val writer = new PrintWriter(file)
    try {
      writer.println("line 1")
      writer.println("line 2")
      writer.println("line 3")
    }
    finally {
      writer.close()
    }

    //when
    reverseLines(file)

    //then
    val lines = Source.fromFile(file).getLines().mkString("\n")
    lines shouldBe "line 3\nline 2\nline 1"
  }
}
