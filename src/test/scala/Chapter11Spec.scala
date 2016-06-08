import Chapter11._
import org.scalatest.{FlatSpec, Matchers}

class Chapter11Spec extends FlatSpec with Matchers {

  "Task1" should "evaluate expressions according to the precedence rules" in {
    //when & then
    (3 + 4 -> 5) shouldBe ((3 + 4) -> 5)
    (3 -> 4 + "5") shouldBe ((3 -> 4) + "5")
  }

  "Fraction" should "be normalized and has operations +, -, *, /" in {
    //when & then
    Fraction(15, -6).toString shouldBe "-5/2"
    (Fraction(1, 2) + Fraction(1, 2)).toString shouldBe "1/1"
    (Fraction(1, 2) - Fraction(1, 2)).toString shouldBe "0/1"
    (Fraction(1, 2) * Fraction(1, 2)).toString shouldBe "1/4"
    (Fraction(1, 2) / Fraction(1, 2)).toString shouldBe "1/1"
    (Fraction(1, 7) - Fraction(2, 9)).toString shouldBe "-5/63"

    //equals
    Fraction(15, -6) == Fraction(15, -6) shouldBe true
    Fraction(15, -6) == Fraction(-15, 6) shouldBe true
    Fraction(15, 6) == Fraction(-15, 6) shouldBe false
    //noinspection ComparingUnrelatedTypes
    Fraction(15, 6).equals("") shouldBe false
    Fraction(15, 6) == null shouldBe false
  }

  "Money" should "be normalized and has operations +, -, ==, <" in {
    //when & then
    Money(1, 150).toString shouldBe "2.50"
    (Money(1, 75) + Money(0, 50)).toString shouldBe "2.25"
    (Money(1, 75) + Money(0, 10)).toString shouldBe "1.85"
    (Money(2, 25) - Money(0, 50)).toString shouldBe "1.75"
    (Money(2, 25) - Money(0, 25)).toString shouldBe "2.00"
    Money(2, 25) == Money(0, 50) shouldBe false
    Money(2, 25) == Money(2, 25) shouldBe true
    Money(2, 25) < Money(2, 25) shouldBe false
    Money(1, 50) < Money(2, 25) shouldBe true
  }

  "Table" should "provide operators that construct an HTML table" in {
    //when
    val table = Table() | "Java" | "Scala" || "Gosling" | "Odersky" || "JVM" | "JVM, .NET"

    //then
    table.toHtml shouldBe "<table><tr><td>Java</td><td>Scala</td></tr>" +
      "<tr><td>Gosling</td><td>Odersky</td></tr>" +
      "<tr><td>JVM</td><td>JVM, .NET</td></tr></table>"
  }

  "ASCIIArt" should "provide operators for combining figures horizontally and vertically" in {
    //given
    val art = new ASCIIArt +
      """ /\_/\""" +
      """( ' ' )""" +
      """(  -  )""" +
      """ | | |""" +
      """(__|__)"""

    //when
    val result = art | new ASCIIArt +
      """    -----""" +
      """  / Hello \""" +
      """ <  Scala |""" +
      """  \ Coder /""" +
      """    -----"""

    //then
    art.toString shouldBe """ /\_/\
                            |( ' ' )
                            |(  -  )
                            | | | |
                            |(__|__)""".stripMargin

    result.toString shouldBe """ /\_/\     -----
                               |( ' ' )  / Hello \
                               |(  -  ) <  Scala |
                               | | | |   \ Coder /
                               |(__|__)    -----""".stripMargin
  }

  "BitSequence" should "store 64 bits, packed in Long" in {
    //given
    val bits = new BitSequence

    //when & then
    bits(0) shouldBe 0
    bits(0) = 1
    bits(0) shouldBe 1
    bits(1) shouldBe 0
    bits(1) = 1
    bits(1) shouldBe 1
    bits(63) shouldBe 0
    bits(63) = 1
    bits(63) shouldBe 1
    bits(0) shouldBe 1
    bits(0) = 0
    bits(0) shouldBe 0
    bits(1) shouldBe 1
    bits(1) = 0
    bits(1) shouldBe 0
    bits(63) shouldBe 1
    bits(63) = 0
    bits(63) shouldBe 0
  }

  "Matrix" should "supply operator + and apply method" in {
    //when
    val result = Matrix(2, 2)(
      1, 2,
      3, 4) +
      Matrix(2, 2)(
        5, 6,
        7, 8)

    //then
    result(0, 0) shouldBe 6
    result(0, 1) shouldBe 8
    result.toString shouldBe """[6, 8]
                               |[10, 12]""".stripMargin
  }

  it should "supply operator * for scalars" in {
    //when
    val result = Matrix(2, 2)(
      1, 2,
      3, 4) * 2

    //then
    result.toString shouldBe """[2, 4]
                               |[6, 8]""".stripMargin
  }

  it should "supply operator *" in {
    //when
    val result = Matrix(3, 2)(
      1, 2,
      3, 4,
      5, 6) *
      Matrix(2, 4)(
        1, 2, 3, 4,
        5, 6, 7, 8)

    //then
    result.toString shouldBe """[11, 14, 17, 20]
                               |[23, 30, 37, 44]
                               |[35, 46, 57, 68]""".stripMargin
  }

  it should "be error tolerant" in {
    //when & then
    a [IllegalArgumentException] should be thrownBy {
      Matrix(1, 0)(1)
    }
    a [IllegalArgumentException] should be thrownBy {
      Matrix(1, 1)(1) + Matrix(1, 2)(1, 1)
    }
    a [IllegalArgumentException] should be thrownBy {
      Matrix(1, 1)(1) * Matrix(2, 1)(1, 1)
    }
  }

  "RichFile" should "extract the file path, name, and extension" in {
    //given
    val file = "file/home/cay/readme.txt"

    //when
    val RichFile(path, name, extension) = file

    //then
    path shouldBe "/home/cay"
    name shouldBe "readme"
    extension shouldBe "txt"
  }

  "RichFile2" should "extract the path sequence" in {
    //given
    val file = "file/home/cay/data/readme.txt"

    //when
    val RichFile2(home, user, data, name) = file

    //then
    home shouldBe "home"
    user shouldBe "cay"
    data shouldBe "data"
    name shouldBe "readme.txt"
  }
}
