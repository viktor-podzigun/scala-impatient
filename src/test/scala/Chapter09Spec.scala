import Chapter09._
import TestUtils.{printToTmpFile, runApp}
import java.io.File
import org.scalatest.{FlatSpec, Matchers}
import scala.io.Source.fromFile

class Chapter09Spec extends FlatSpec with Matchers {

  "reverseLines" should "reverse lines in file" in {
    //given
    val file = printToTmpFile("reverseLines", """line 1
                                                |line 2
                                                |line 3
                                                |""".stripMargin)

    //when
    reverseLines(file)

    //then
    fromFile(file).mkString shouldBe """line 3
                                       |line 2
                                       |line 1
                                       |""".stripMargin
  }

  "replaceTabs" should "replace tabs with spaces using column boundaries" in {
    //given
    val file = printToTmpFile("replaceTabs", """text	text2	text3
                                               |	text	text2	text3
                                               |		text	text2	text3
                                               |""".stripMargin)

    //when
    replaceTabs(file, 2)

    //then
    fromFile(file).mkString shouldBe """text  text2 text3
                                       |  text  text2 text3
                                       |    text  text2 text3
                                       |""".stripMargin
  }

  "printLongWords" should "read a file and print all words longer than 12 chars" in {
    //given
    val file = printToTmpFile("printLongWords", "text toooooooolong text2 text3texttext2text3")

    //when
    val (exit, out, err) = runApp("PrintLongWordsApp", file.getAbsolutePath)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """toooooooolong
                   |text3texttext2text3
                   |""".stripMargin
  }

  "printNumbersStat" should "read numbers from file and print sum, average, min, max" in {
    //given
    val file = printToTmpFile("printNumbersStat", "1 1.2 2.34 -5 25.5 0.0 1.234")

    //when
    val (exit, out, err) = runApp("PrintNumbersStatApp", file.getAbsolutePath)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """sum:     26.274
                   |average: 3.753
                   |minimum: -5.000
                   |maximum: 25.500
                   |""".stripMargin
  }

  "printPowersOf2" should "print powers of 2 and their reciprocals to a file" in {
    //given
    val file = File.createTempFile("printPowersOf2", "txt")

    //when
    printPowersOf2(file)

    //then
    val res = new StringBuilder
    for (i <- 0 to 20) {
      res.append("%8.0f  %f\n".format(math.pow(2.0, i), math.pow(2.0, -i)))
    }

    fromFile(file).mkString shouldBe res.toString
  }

  "printQuotedStrings" should "read numbers from file and print sum, average, min, max" in {
    //given
    val file = "src/main/scala/Chapter09.scala"

    //when
    val (exit, out, err) = runApp("PrintQuotedStringsApp", file)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """\\s+
                   |\\s+
                   |sum:     %.3f\n
                   |average: %.3f\n
                   |minimum: %.3f\n
                   |maximum: %.3f\n
                   |%8.0f  %f
                   |like this, maybe with \" or \\
                   |\"(([^\\\\\"]+|\\\\([btnfr\"'\\\\]|[0-3]?[0-7]{1,2}|u[0-9a-fA-F]{4}))*)\"
                   |(?![\\d]+(\\.[\\d]+)?)\\w+
                   |(?i)<img\\s+(.*?\\s+)?src\\s*=\\s*\"([^\"]+)\"
                   |Expect file name as first argument
                   |""".stripMargin
  }

  "printNonNumberTokens" should "print tokens from file that are not floating-point numbers" in {
    //given
    val file = printToTmpFile("printNonNumberTokens", "1 first 2.34 second -5 0.0 1.234 third")

    //when
    val (exit, out, err) = runApp("PrintNonNumberTokensApp", file.getAbsolutePath)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """first
                   |second
                   |third
                   |""".stripMargin
  }

  "printSrcOfImageTags" should "replace tabs with spaces using column boundaries" in {
    //given
    val file = printToTmpFile("printSrcOfImageTags",
      """<!DOCTYPE html>
        |<html>
        |<body>
        |  <IMG alt="scala logo" src="http://scala-lang.org/resources/img/scala-logo-white.png"/>
        |</body>
        |</html>
        |""".stripMargin)

    //when
    val (exit, out, err) = runApp("PrintSrcOfImageTagsApp", file.getAbsolutePath)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """http://scala-lang.org/resources/img/scala-logo-white.png
                   |""".stripMargin
  }
}
