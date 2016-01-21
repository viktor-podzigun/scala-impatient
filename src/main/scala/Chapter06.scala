
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
}
