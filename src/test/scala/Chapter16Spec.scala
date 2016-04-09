import Chapter16._
import TestUtils.runApp
import org.scalatest.{FlatSpec, Matchers}
import scala.xml.{XML, Elem, Text}

class Chapter16Spec extends FlatSpec with Matchers {

  "task1" should "check results" in {
    //when & then
    <fred/>(0).toString() shouldBe "<fred/>"
    <fred/>(0)(0).toString() shouldBe "<fred/>"
    <fred/>(0)(0)(0).toString() shouldBe "<fred/>"
    <fred/>(0)(0)(0)(0).toString() shouldBe "<fred/>"
  }

  "task2" should "escape special characters" in {
    //when & then
    val result = <ul>
      <li>Opening bracket: [</li>
      <li>Closing bracket: ]</li>
      <li>Opening brace: {{</li>
      <li>Closing brace: }}</li>
    </ul>

    result.toString() shouldBe """<ul>
                                 |      <li>Opening bracket: [</li>
                                 |      <li>Closing bracket: ]</li>
                                 |      <li>Opening brace: {</li>
                                 |      <li>Closing brace: }</li>
                                 |    </ul>""".stripMargin
  }

  "task3" should "check expressions" in {
    //when & then
    val res1 = <li>Fred</li> match { case <li>{Text(t)}</li> => t }
    res1.toString shouldBe "Fred"

    a [scala.MatchError] should be thrownBy {
      <li>{"Fred"}</li> match { case <li>{Text(t)}</li> => t }
    }

    val res2 = <li>{Text("Fred")}</li> match { case <li>{Text(t)}</li> => t }
    res2.toString shouldBe "Fred"
  }

  "printImgWithoutAlt" should "print all img elements without alt attribute" in {
    //given
    val file = "/Chapter16Task04.html"

    //when
    val (exit, out, err) = runApp("Chapter16PrintImgWithoutAltApp", file)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """<img src="http://test.com/img1.png"/>
                   |<img src="http://test.com/img2.png"/>
                   |""".stripMargin
  }

  "printAllImg" should "print all img src attributes" in {
    //given
    val file = "/Chapter16Task04.html"

    //when
    val (exit, out, err) = runApp("Chapter16PrintAllImgApp", file)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """http://test.com/img1.png
                   |http://test.com/img2.png
                   |http://test.com/img3.png
                   |""".stripMargin
  }

  "printAllHyperlinks" should "print a table of all hyperlinks together with their URLs" in {
    //given
    val file = "/Chapter16Task04.html"

    //when
    val (exit, out, err) = runApp("Chapter16PrintAllHyperlinksApp", file)

    //then
    exit shouldBe 0
    err shouldBe ""
    out shouldBe """+---------------------------------------+----------------------+
                   || <img src="http://test.com/img1.png"/> | http://test.com/ref1 |
                   || Ref 2                                 | http://test.com/ref2 |
                   || Ref 3                                 | http://test.com/ref3 |
                   |+---------------------------------------+----------------------+
                   |""".stripMargin
  }

  "mapToXml" should "produce XML from the given map" in {
    //given
    val map = Map("A" -> "1", "B" -> "2")

    //when
    val result: Elem = mapToXml(map)

    //then
    result shouldBe <dl><dt>A</dt><dd>1</dd><dt>B</dt><dd>2</dd></dl>
  }

  "xmlToMap" should "produce map from the given Xml" in {
    //given
    val xml = <dl><dt>A</dt><dd>1</dd><dt>B</dt><dd>2</dd></dl>

    //when
    val result: Map[String, String] = xmlToMap(xml)

    //then
    result shouldBe Map("A" -> "1", "B" -> "2")
  }

  "transformXhtml" should "transform an XHTML document" in {
    //given
    val root = XML.load(getClass.getResourceAsStream("/Chapter16Task04.html"))

    //when
    val result: Elem = transformXhtml(root)

    //then
    result.toString() shouldBe """<html>
                                 |    <body>
                                 |        <p>
                                 |            <a href="http://test.com/ref1">
                                 |                <img alt="TODO" src="http://test.com/img1.png"/>
                                 |            </a>
                                 |        </p>
                                 |        <a href="http://test.com/ref2">Ref 2</a>
                                 |        <div>
                                 |            <img alt="TODO" src="http://test.com/img2.png"/>
                                 |
                                 |            <a href="http://test.com/ref3">Ref 3</a>
                                 |        </div>
                                 |        <img src="http://test.com/img3.png" alt="Some text"/>
                                 |    </body>
                                 |</html>""".stripMargin
  }
}
