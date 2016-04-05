import Chapter16._
import org.scalatest.{FlatSpec, Matchers}
import scala.xml.{Text, Node, Elem}

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
}
