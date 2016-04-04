import Chapter16._
import org.scalatest.{FlatSpec, Matchers}

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
}
