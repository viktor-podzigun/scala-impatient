import Chapter06._
import org.scalatest.{FlatSpec, Matchers}

class Chapter06Spec extends FlatSpec with Matchers {

  "Conversions" should "not turn negative at Int.MaxValue" in {
    Conversions.inchesToCentimeters(1).formatted("%.2f") shouldBe "2.54"
    Conversions.gallonsToLiters(1).formatted("%.3f") shouldBe "3.785"
    Conversions.milesToKilometers(1).formatted("%.6f") shouldBe "1.609347"
  }
}
