import Chapter11.Fraction
import Chapter21._
import TestUtils.withOutputAndResult
import java.awt.Point
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

  "smaller" should "work with Fraction instances" in {
    //when & then
    smaller(Fraction(1, 7), Fraction(2, 9)) shouldBe Fraction(1, 7)
    smaller(Fraction(1, 7), Fraction(1, 7)) shouldBe Fraction(1, 7)
    smaller(Fraction(0, 7), Fraction(0, 9)) shouldBe Fraction(0, 9)
  }

  "RichPoint" should "compare java.awt.Point objects by lexicographic comparison" in {
    //when & then
    new Point(1, 1) shouldBe new Point(1, 1)
    new Point(1, 1) should be < new Point(2, 1)
    new Point(1, 1) should be < new Point(1, 2)
  }
}
