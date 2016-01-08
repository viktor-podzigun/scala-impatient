import Chapter04._
import org.scalatest.{Matchers, FlatSpec}

class Chapter04Spec extends FlatSpec with Matchers {

  "Chapter04" should "set a to an array of n random integers" in {
    val gizmos: Map[String, Int] = gizmosWithReducedPrice()
    gizmos("iPhone") should be (540)
    gizmos("iPad") should be (450)
    gizmos("MacBook Pro") should be (1800)
    gizmos("ScalaDays 2016 Berlin") should be (675)
  }
}
