import Chapter06._
import org.scalatest.{FlatSpec, Matchers}

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
}
