import java.util.{Calendar, Date}
import scala.util.parsing.combinator.RegexParsers
import scala.xml._

object Chapter19 {

  /**
   * Task 1:
   *
   * Add `/` and `%` operations to the arithmetic expression evaluator.
   */
  class ExprEvaluator extends RegexParsers {

    val number = "[0-9]+".r

    def eval(e: String): Int = parseAll(expr, e).get

    def expr: Parser[Int] = term ~ rep(("+" | "-") ~ term ^^ {
      case "+" ~ t => t
      case "-" ~ t => -t
    }) ^^ {
      case t ~ r => t + r.sum
    }

    def term: Parser[Int] = factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
      case f ~ r => r.foldLeft(f)((b, a) => a._1 match {
        case "*" => b * a._2
        case "/" => b / a._2
        case "%" => b % a._2
      })
    }

    def factor: Parser[Int] = number ^^ { _.toInt } | "(" ~ expr ~ ")" ^^ {
      case _ ~ e ~ _ => e
    }
  }

  /**
   * Task 2:
   *
   * Add a `^` operator to the arithmetic expression evaluator. As in mathematics, `^` should have
   * a higher precedence than multiplication, and it should be right associative.
   * That is, `4^2^3` should be `4^(2^3)`, or `65536`.
   */
  class ExprEvaluator2 extends RegexParsers {

    val number = "[0-9]+".r

    def eval(e: String): Int = parseAll(expr, e).get

    def expr: Parser[Int] = term ~ rep(("+" | "-") ~ term ^^ {
      case "+" ~ t => t
      case "-" ~ t => -t
    }) ^^ {
      case t ~ r => t + r.sum
    }

    def term: Parser[Int] = power ~ rep(("*" | "/" | "%") ~ power) ^^ {
      case p ~ r => r.foldLeft(p)((b, a) => a._1 match {
        case "*" => b * a._2
        case "/" => b / a._2
        case "%" => b % a._2
      })
    }

    def power: Parser[Int] = rep(factor ~ "^") ~ factor ^^ {
      case r ~ f => r.foldRight(f)((a, b) => a._2 match {
        case "^" => math.pow(a._1, b).toInt
      })
    }

    def factor: Parser[Int] = number ^^ { _.toInt } | "(" ~ expr ~ ")" ^^ {
      case _ ~ e ~ _ => e
    }
  }

  /**
   * Task 3:
   *
   * Write a parser that parses a list of integers (such as `(1, 23, -79)`) into a `List[Int]`.
   */
  class IntListParser extends RegexParsers {

    val number = "-?[0-9]+".r

    def parse(e: String): List[Int] = {
      val result = parseAll(list, e)
      if (!result.successful) {
        throw new RuntimeException("Parsing failed: " + result)
      }

      result.get
    }

    def list: Parser[List[Int]] = "(" ~> repsep(number ^^ { _.toInt }, ",") <~ ")"
  }

  /**
   * Task 4:
   *
   * Write a parser that can parse date and time expressions in ISO 8601.
   * Your parser should return a `java.util.Date` object.
   */
  class DateTimeParser extends RegexParsers {

    val numRegex = """\d{2}""".r
    val millisRegex = """\d{3}""".r
    val yearRegex = """\d{4}""".r

    def parse(e: String): Date = {
      val result = parseAll(dateTime, e)
      if (!result.successful) {
        throw new RuntimeException("Parsing failed: " + result)
      }

      result.get
    }

    def dateTime: Parser[Date] = date ~ opt(time) ^^ {
      case (y, m, d) ~ None => makeDate(y, m, d, 0, 0, 0, 0)
      case (y, m, d) ~ Some((h, mm, s, ss)) => makeDate(y, m, d, h, mm, s, ss)
    }

    def date: Parser[(Int, Int, Int)] = year ~ month ~ day ^^ {
      case y ~ m ~ d => (y, m, d)
    }

    def time: Parser[(Int, Int, Int, Int)] = "T" ~> hour ~ minute ~ second ~ opt(millis) ^^ {
      case h ~ m ~ s ~ None => (h, m, s, 0)
      case h ~ m ~ s ~ Some(ss) => (h, m, s, ss)
    }

    def year: Parser[Int] = yearRegex ^^ { _.toInt }
    def month: Parser[Int] = opt("-") ~> numRegex ^^ { _.toInt }
    def day: Parser[Int] = opt("-") ~> numRegex ^^ { _.toInt }
    def hour: Parser[Int] = numRegex ^^ { _.toInt }
    def minute: Parser[Int] = opt(":") ~> numRegex ^^ { _.toInt }
    def second: Parser[Int] = opt(":") ~> numRegex ^^ { _.toInt }
    def millis: Parser[Int] = opt(".") ~> millisRegex ^^ { _.toInt }

    private def makeDate(y: Int, m: Int, d: Int, h: Int, mm: Int, s: Int, ss: Int): Date = {
      val cal = Calendar.getInstance()
      cal.set(y, m - 1, d, h, mm, s)
      cal.set(Calendar.MILLISECOND, ss)
      cal.getTime
    }
  }

  /**
   * Task 5:
   *
   * Write a parser that parses a subset of XML. Handle tags of the form `<ident>...</ident>` or
   * `<ident/>`. Tags can be nested. Handle attributes inside tags. Attribute values can be
   * delimited by single or double quotes. You don't need to deal with character data
   * (that is, text inside tags or CDATA sections).
   * Your parser should return a Scala XML `Elem` value.
   * The challenge is to reject mismatched tags. Hint: `into`, `accept`.
   */
  class IdentXMLParser extends RegexParsers {

    val attrKeyRegex = """[^</>='"]+""".r
    val singleQuotesRegex = """[^']+""".r
    val doubleQuotesRegex = """[^"]+""".r
    val textRegex = """[^<>]+""".r
    val cdataRegex = """(?s)<!\[CDATA\[.*?\]\]>""".r

    def parse(e: String): xml.Elem = {
      val result = parseAll(openCloseTag, e)
      if (!result.successful) {
        throw new IllegalArgumentException("Parsing failed: " + result)
      }

      result.get
    }

    def openCloseTag: Parser[xml.Elem] = tagOpen into { elem =>
      opt(textRegex) ~> rep(singleTag <~ opt(textRegex) | openCloseTag <~ opt(textRegex)) <~
        tagClose ^^ {
        case Nil => elem
        case r => elem.copy(child = r.filter(_ != null))
      }
    }

    def tagOpen: Parser[xml.Elem] = ("<" ~ tagName) ~> attrs <~ ">"
    def tagClose: Parser[String] = "</" ~> tagName <~ ">"

    def singleTag: Parser[xml.Elem] = ("<" ~ tagName) ~> attrs <~ "/>" | cdataRegex ^^ { _ => null }

    def tagName: Parser[String] = "ident" | failure("ident tag expected")

    def attrs: Parser[xml.Elem] = rep(attrPair) ^^ {
      case Nil => makeElem(Nil)
      case attrPairs => makeElem(attrPairs)
    }

    def makeElem(attrPairs: List[(String, String)]): xml.Elem = {
      val attributes = if (attrPairs.nonEmpty) {
        val attrs = for ((key, value) <- attrPairs) yield Attribute(null, key, value, Null)
        attrs.reduceRight((attr, next) => attr.copy(next = next))
      }
      else Null

      xml.Elem(null, "ident", attributes, TopScope, minimizeEmpty = false)
    }

    def attrPair: Parser[(String, String)] = attrKeyRegex ~ "=" ~ attrValue ^^ {
      case key ~ "=" ~ value => (key, value)
    }

    def attrValue: Parser[String] = "'" ~> singleQuotesRegex <~ "'" |
      "\"" ~> doubleQuotesRegex <~ "\""
  }
}
