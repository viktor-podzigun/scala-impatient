import org.scalatest.{FlatSpec, Matchers}
import task0604.Point

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
    //when
    val (exit: Int, out: String, err: String) = TestUtils.runApp("Reverse", "Hello", "World")

    //then
    exit shouldBe 0
    out shouldBe "World Hello\n"
    err shouldBe ""
  }

  "PlayingCard" should "describe the four playing card suits" in {
    PlayingCard.Clubs.toString shouldBe "♣"
    PlayingCard.Diamonds.toString shouldBe "♦"
    PlayingCard.Hearts.toString shouldBe "♥"
    PlayingCard.Spades.toString shouldBe "♠"
  }

  it should "implement isRed function" in {
    PlayingCard.isRed(PlayingCard.Clubs) shouldBe false
    PlayingCard.isRed(PlayingCard.Diamonds) shouldBe true
    PlayingCard.isRed(PlayingCard.Hearts) shouldBe true
    PlayingCard.isRed(PlayingCard.Spades) shouldBe false
  }

  "RGB" should "describe the eight corners of the RGB color cube" in {
    assertRGB(RGB.Black, 0x000000, "Black")
    assertRGB(RGB.White, 0xffffff, "White")
    assertRGB(RGB.Red, 0xff0000, "Red")
    assertRGB(RGB.Lime, 0x00ff00, "Lime")
    assertRGB(RGB.Blue, 0x0000ff, "Blue")
    assertRGB(RGB.Yellow, 0xffff00, "Yellow")
    assertRGB(RGB.Cyan, 0x00ffff, "Cyan")
    assertRGB(RGB.Magenta, 0xff00ff, "Magenta")
  }

  private def assertRGB(rgb: RGB.Value, id: Int, name: String): Unit = {
    rgb.id shouldBe id
    rgb.toString shouldBe name
  }
}
