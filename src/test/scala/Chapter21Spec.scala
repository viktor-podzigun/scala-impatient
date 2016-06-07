import Chapter21._
import TestUtils.withOutputAndResult
import java.io.StringReader
import org.scalatest.{FlatSpec, Matchers}
import scala.language.postfixOps

class Chapter21Spec extends FlatSpec with Matchers {

  "PercentAdder" should "define an operator `+%` and use implicit conversion" in {
    120 +% 10 shouldBe 132
  }

  "Int2Factorial" should "define an operator `!` and use implicit conversion" in {
    (5!) shouldBe 120
  }

  "FluentReader" should "provide fluent APIs for reading data from the console" in {
    //given
    val input = """Viktor
                  |35
                  |80
                  |""".stripMargin

    //when
    val (out: String, result: FluentReader) = withOutputAndResult {
      Console.withIn(new StringReader(input)) {
        Read in aString askingFor "Your name" and
          anInt askingFor "Your age" and
          aDouble askingFor "Your weight"
      }
    }

    //then
    result.getData.mkString("\n") shouldBe """(Your name,Viktor)
                                             |(Your age,35)
                                             |(Your weight,80.0)""".stripMargin

    out shouldBe """Your name: Your age: Your weight: """.stripMargin
  }
}
