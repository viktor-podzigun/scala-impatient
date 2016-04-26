import scala.util.parsing.combinator.RegexParsers

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
}
