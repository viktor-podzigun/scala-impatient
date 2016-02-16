import Chapter10._
import org.scalatest.{FlatSpec, Matchers}

class Chapter10Spec extends FlatSpec with Matchers {

  "RectangleLike" should "provide translate and grow methods" in {
    //given
    val egg = new java.awt.geom.Ellipse2D.Double(5, 10, 20, 30) with RectangleLike

    //when & then
    egg.translate(10, -10)
    egg.getX shouldBe 15
    egg.getY shouldBe 0
    egg.getWidth shouldBe 20
    egg.getHeight shouldBe 30

    //when & then
    egg.grow(10, 20)
    egg.getX shouldBe 5
    egg.getY shouldBe -20
    egg.getWidth shouldBe 40
    egg.getHeight shouldBe 70
  }

  "OrderedPoint" should "use lexicographic ordering" in {
    //when & then
    new OrderedPoint(1, 1).compare(new OrderedPoint(2, 2)) shouldBe -1
    new OrderedPoint(1, 1).compare(new OrderedPoint(2, 1)) shouldBe -1
    new OrderedPoint(1, 1).compare(new OrderedPoint(1, 2)) shouldBe -1
    new OrderedPoint(1, 1).compare(new OrderedPoint(1, 1)) shouldBe 0
    new OrderedPoint(1, 1).compare(new OrderedPoint(0, 1)) shouldBe 1
    new OrderedPoint(1, 1).compare(new OrderedPoint(1, 0)) shouldBe 1
    new OrderedPoint(1, 1).compare(new OrderedPoint(0, 0)) shouldBe 1
  }
}
