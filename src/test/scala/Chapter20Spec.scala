import Chapter20._
import org.scalatest.{FlatSpec, Matchers}

class Chapter20Spec extends FlatSpec with Matchers {

  "RandCalc" should "compute the average of random numbers" in {
    //given
    val n = 1000000

    //when
    val average: Double = RandCalc.calcAverageFor(n, useActors = true)

    //then
    average should be > 0.0
  }
}
