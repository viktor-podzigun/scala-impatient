import Chapter19._
import TestUtils.withOutput
import java.util.{Calendar, Date}
import org.scalatest.{FlatSpec, Matchers}

class Chapter19Spec extends FlatSpec with Matchers {

  "ExprEvaluator" should "handle / and % operations" in {
    //given
    val e = new ExprEvaluator

    //when & then
    e.eval("3 - 4 * 5") shouldBe -17
    e.eval("3 - 8 / 4 * 5 + 3 % 4") shouldBe -4
  }

  "ExprEvaluator2" should "handle ^ operator" in {
    //given
    val e = new ExprEvaluator2

    //when & then
    e.eval("3 - 4 * 5") shouldBe -17
    e.eval("3 - 8 / 4 * 5 + 3 % 4") shouldBe -4
    e.eval("4 ^ 2 ^ 3") shouldBe 65536
    e.eval("1 + 2 * 4 ^ 2 ^ 3 * 2 - 1") shouldBe 262144
  }

  "IntListParser" should "parse a list of integers into a List[Int]" in {
    //given
    val p = new IntListParser

    //when & then
    p.parse("()") shouldBe List()
    p.parse("(1)") shouldBe List(1)
    p.parse("(1, 23, -79)") shouldBe List(1, 23, -79)
  }

  "DateTimeParser" should "date and time expressions in ISO 8601" in {
    //given
    val p = new DateTimeParser

    //when & then
    p.parse("2005-08-09T18:31:42.123") shouldBe date(2005, 8, 9, 18, 31, 42, 123)
    p.parse("20050809T183142123") shouldBe date(2005, 8, 9, 18, 31, 42, 123)
    p.parse("2005-08-09T18:31:42") shouldBe date(2005, 8, 9, 18, 31, 42, 0)
    p.parse("20050809T183142") shouldBe date(2005, 8, 9, 18, 31, 42, 0)
    p.parse("2005-08-09") shouldBe date(2005, 8, 9, 0, 0, 0, 0)
    p.parse("20050809") shouldBe date(2005, 8, 9, 0, 0, 0, 0)
  }

  "IdentXMLParser" should "parse a subset of XML" in {
    //given
    val p = new IdentXMLParser

    //when & then
    a [IllegalArgumentException] should be thrownBy {
      p.parse("<a></a>")
    }
    a [IllegalArgumentException] should be thrownBy {
      p.parse("<ident></a>")
    }
    a [IllegalArgumentException] should be thrownBy {
      p.parse("<a></ident>")
    }
    a [IllegalArgumentException] should be thrownBy {
      p.parse("<ident><a/></ident>")
    }
    a [IllegalArgumentException] should be thrownBy {
      p.parse("<ident><a></a></ident>")
    }
    a [IllegalArgumentException] should be thrownBy {
      p.parse("<ident><ident/><a/></ident>")
    }
    a [IllegalArgumentException] should be thrownBy {
      p.parse("<ident/>")
    }

    p.parse("""<ident>
              |  text <![CDATA[
              |  <ident/>]]> text <![CDATA[<ident/>]]> text
              |</ident>
              |""".stripMargin) shouldBe <ident></ident>

    p.parse("""<ident a1="val1" a2="val2" a3='val3'>
              |  text
              |  text text
              |</ident>
              |""".stripMargin) shouldBe <ident a1="val1" a2="val2" a3='val3'></ident>

    p.parse( """<ident a1="val1"> a
               |  <ident a2="val2"> b </ident>c
               |  <ident a3='val3'/>d
               |</ident>""".stripMargin) shouldBe
      <ident a1="val1"><ident a2="val2"></ident><ident a3='val3'/></ident>
  }

  "ExprParser" should "produce correct expression tree" in {
    //given
    val p = new ExprParser

    //when & then
    p.parse("3-4-5") shouldBe Operator("-", Operator("-", Number(3), Number(4)), Number(5))
    p.parse("3-4+5") shouldBe Operator("+", Operator("-", Number(3), Number(4)), Number(5))
    p.parse("3-4*5") shouldBe Operator("-", Number(3), Operator("*", Number(4), Number(5)))
  }

  "FoldExprEvaluator" should "implement expression computation as a fold" in {
    //given
    val p = new FoldExprEvaluator

    //when & then
    p.parse("3-4+5") shouldBe 4
    p.parse("3+4-5") shouldBe 2
    p.parse("3-4*5") shouldBe -17
  }

  "Calculator" should "use variables and assignment when calculating expressions" in {
    //given
    val c = new Calculator

    //when & then
    c.parseAndEval("") shouldBe 0
    c.parseAndEval("3-4+5+a") shouldBe 4
    c.parseAndEval("""a=6
                     |3-4+5""".stripMargin) shouldBe 4
    c.parseAndEval("""a=6+1
                     |3-4+5+a""".stripMargin) shouldBe 11
    c.parseAndEval("""a=b
                     |3-4+5+a""".stripMargin) shouldBe 4
    c.parseAndEval("""a=b-6
                     |3-4+5+a""".stripMargin) shouldBe -2
    withOutput {
      c.parseAndEval("""a=(3+3)
                       |out=a+1""".stripMargin)
    } shouldBe "7"
  }

  "Program" should "use variable assignments, Boolean expressions, if/else, while statements" in {
    //given
    val p = new Program

    //when & then
    p.parseAndEval("""a=b-6
                     |3-4+5+a
                     |""".stripMargin) shouldBe -2
    p.parseAndEval("""
                     |a=b-6
                     |
                     |if (1 <= 2) {
                     |  3-4+5+a
                     |}
                     |else {
                     |  5
                     |}
                     |""".stripMargin) shouldBe -2
    p.parseAndEval("""a=b-6
                     |if (1 > 2)
                     |{
                     |  //3-4+5+a
                     |}
                     |else
                     |{
                     |  5
                     |}
                     |""".stripMargin) shouldBe 5
    p.parseAndEval("""a=b-6
                     |if (1 > 2) {
                     |  3-4+5+a
                     |} else {
                     |  a = 0
                     |  while (a < 10) {
                     |    a = a + 1
                     |  }
                     |  a
                     |}
                     |""".stripMargin) shouldBe 10
    withOutput {
      p.parseAndEval("""a=(3+3)
                       |if (a < 10) {
                       |  out=a+1
                       |}""".stripMargin)
    } shouldBe "7"
  }

  "FuncProgram" should "add function definitions to program evaluator" in {
    //given
    val f = new FuncProgram

    //when & then
    the [IllegalArgumentException] thrownBy {
      f.parseAndEval("""a()
                       |""".stripMargin)
    } should have message """function definition not found at 1.1
                            |a()
                            |
                            |^""".stripMargin
    the [IllegalArgumentException] thrownBy {
      f.parseAndEval("""def a() {
                       |}
                       |a(1)
                       |""".stripMargin)
    } should have message """function call with wrong arguments number at 3.1
                            |a(1)
                            |
                            |^""".stripMargin
    the [IllegalArgumentException] thrownBy {
      f.parseAndEval("""def a(b) {
                       |}
                       |a()
                       |""".stripMargin)
    } should have message """function call with wrong arguments number at 3.1
                            |a()
                            |
                            |^""".stripMargin
    f.parseAndEval("""
                     |def a() {
                     |  b-6
                     |}
                     |
                     |a = 3-4
                     |if (1 <= 2) {
                     |  a+5+a()
                     |}
                     |else {
                     |  5
                     |}
                     |""".stripMargin) shouldBe -2
    f.parseAndEval("""
                     |def a(b) {
                     |  b = b-6
                     |  if (1 <= 2) {
                     |    3-4+5+b
                     |  }
                     |}
                     |a(1)
                     |a(2)
                     |a(3)
                     |""".stripMargin) shouldBe 1
    withOutput {
      f.parseAndEval("""def a(a, c) {
                       |  out=a+c+1
                       |}
                       |a = 2
                       |a(3+3, a)
                       |""".stripMargin)
    } shouldBe "9"
  }

  private def date(y: Int, m: Int, d: Int, hh: Int, mm: Int, ss: Int, sss: Int): Date = {
    val cal = Calendar.getInstance()
    cal.set(y, m - 1, d, hh, mm, ss)
    cal.set(Calendar.MILLISECOND, sss)
    cal.getTime
  }
}
