
object Chapter06 {

  /**
   * Task 1:
   *  Write an object Conversions with methods inchesToCentimeters, gallonsToLiters,
   *  and milesToKilometers.
   */
  object Conversions {

    def inchesToCentimeters(inches: Double): Double = inches * 2.54

    def gallonsToLiters(gallons: Double): Double = gallons * 3.785

    def milesToKilometers(miles: Double): Double = miles / 0.62137
  }

  /**
   * Task 2:
   *  The preceding problem wasn't very object-oriented. Provide a general super-class
   *  UnitConversion and define objects InchesToCentimeters, GallonsToLiters, and
   *  MilesToKilometers that extend it.
   */
  abstract class UnitConversion {

    def convert(inches: Double): Double
  }

  object InchesToCentimeters extends UnitConversion {

    override def convert(inches: Double) = inches * 2.54
  }

  object GallonsToLiters extends UnitConversion {

    override def convert(gallons: Double) = gallons * 3.785
  }

  object MilesToKilometers extends UnitConversion {

    override def convert(miles: Double) = miles / 0.62137
  }
}
