import java.io.{File, PrintWriter}

import Chapter09._
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source.fromFile

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
    fromFile(file).mkString shouldBe "line 3\nline 2\nline 1\n"
  }

  "replaceTabs" should "replace tabs with spaces using column boundaries" in {
    //given
    val file = File.createTempFile("replaceTabs", "txt")
    val writer = new PrintWriter(file)
    try {
      writer.print("""text	text2	text3
                     |	text	text2	text3
                     |		text	text2	text3
                     |""".stripMargin)
    }
    finally {
      writer.close()
    }

    //when
    replaceTabs(file, 3)

    //then
    fromFile(file).mkString shouldBe """text  text2 text3
                                       |   text  text2 text3
                                       |      text  text2 text3
                                       |""".stripMargin
  }
}
