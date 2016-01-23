import java.io.File

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class Chapter06Spec extends FlatSpec with Matchers {

  "Conversions" should "convert units using separate methods" in {
    Conversions.inchesToCentimeters(1).formatted("%.2f") shouldBe "2.54"
    Conversions.gallonsToLiters(1).formatted("%.3f") shouldBe "3.785"
    Conversions.milesToKilometers(1).formatted("%.6f") shouldBe "1.609347"
  }

  "UnitConversion" should "convert units using general super-class" in {
    val inchesToCentimeters: UnitConversion = InchesToCentimeters
    inchesToCentimeters.convert(1).formatted("%.2f") shouldBe "2.54"

    val gallonsToLiters: UnitConversion = GallonsToLiters
    gallonsToLiters.convert(1).formatted("%.3f") shouldBe "3.785"

    val milesToKilometers: UnitConversion = MilesToKilometers
    milesToKilometers.convert(1).formatted("%.6f") shouldBe "1.609347"
  }

  "Point" should "construct instances without using new" in {
    val point = Point(3, 4)
    point.x shouldBe 3
    point.y shouldBe 4
  }

  "Reverse" should "print the command-line arguments in reverse order" in {
    //given
    val processBuilder = new ProcessBuilder("java",
      "-cp", System.getProperty("user.home") +
        "/.m2/repository/org/scala-lang/scala-library/2.11.7/scala-library-2.11.7.jar" +
        File.pathSeparator + "./target/classes",
      "Reverse", "Hello", "World")
    processBuilder.redirectErrorStream(true)

    //when
    val process = processBuilder.start()
    val result = Source.fromInputStream(process.getInputStream).getLines().mkString("\n")
    process.waitFor()

    //then
    process.exitValue() shouldBe 0
    result shouldBe "World Hello"
  }

  "PlayingCard" should "describe the four playing card suits" in {
    PlayingCard.Clubs.toString shouldBe "♣"
    PlayingCard.Diams.toString shouldBe "♦"
    PlayingCard.Hearts.toString shouldBe "♥"
    PlayingCard.Spades.toString shouldBe "♠"
  }
}
