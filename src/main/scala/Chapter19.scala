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
}
