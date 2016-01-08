

object Chapter04 {

  /**
   * Task 1:
   *   Set up a map of prices for a number of gizmos that you covet.
   *   Then produce a second map with the same keys and the prices at a 10 percent discount.
   */
  def gizmosWithReducedPrice(): Map[String, Int] = {
    val gizmos = Map("iPhone" -> 600,
      "iPad" -> 500,
      "MacBook Pro" -> 2000,
      "ScalaDays 2016 Berlin" -> 750)

    gizmos.mapValues(price => price - (price / 10))
  }
}
