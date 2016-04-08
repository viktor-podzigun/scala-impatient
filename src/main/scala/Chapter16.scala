import scala.collection.mutable
import scala.xml.{Elem, Text, XML}

object Chapter16 {

  /**
   * Task 1:
   *
   * What is `<fred/>(0)`? `<fred/>(0)(0)`? Why?
   *
   * Solution:
   *
   * Since `Elem` extends `Node` and `Node` extends `NodeSeq`, which is in turn extends
   * `Seq[Node]`, in other words an XML element is represented as node sequence of one item.
   * So, expression `elem(0)` will always return the same `elem`:
   * {{{
   *   elem(0) == elem
   *   elem(0)(0) == elem
   *   elem(0)(0)(0) == elem
   *   ...
   * }}}
   */

  /**
   * Task 2:
   *
   * What is the result of
   * {{{
   *  <ul>
   *    <li>Opening bracket: [</li>
   *    <li>Closing bracket: ]</li>
   *    <li>Opening brace: {</li>
   *    <li>Closing brace: }</li>
   *  </ul>
   * }}}
   * How do you fix it?
   *
   * Solution:
   *
   * The given snippet produces an error in the third `li` element: "No closing Tag", because
   * the brace `{` symbol is interpreted by Scala compiler as start of Scala expression.
   * To fix it we can escape braces by using '{{' and '}}'.
   *
   * @see Chapter16Spec.scala
   */

  /**
   * Task 3:
   *
   * Contrast
   * {{{
   *  <li>Fred</li> match { case <li>{Text(t)}</li> => t }
   * }}}
   * and
   * {{{
   *  <li>{"Fred"}</li> match { case <li>{Text(t)}</li> => t }
   * }}}
   * Why do they act differently?
   *
   * Solution:
   *
   * Since embedded strings, like `{"Fred"}` don't get turned into `Text` nodes we cannot
   * properly pattern match using `Text` node. That's why our second expression failed.
   * To fix it we should either rewrite our patten match expression or we can wrap embedded strings
   * into `Text` node:
   * {{{
   *  <li>{Text("Fred")}</li> match { case <li>{Text(t)}</li> => t }
   * }}}
   */

  /**
   * Task 4:
   *
   * Read an XHTML file and print all `img` elements that don't have an `alt` attribute.
   */
  def printImgWithoutAlt(file: String): Unit = {
    val root = XML.load(getClass.getResourceAsStream(file))
    for (n <- root \\ "img" if n.attribute("alt").isEmpty) {
      println(n)
    }
  }

  /**
   * Task 5:
   *
   * Print the names of all images in an XHTML file. That is, print all `src` attribute values
   * inside `img` elements.
   */
  def printAllImg(file: String): Unit = {
    val root = XML.load(getClass.getResourceAsStream(file))
    for (n <- root \\ "img";
         src <- n.attribute("src")) {

      println(src.text)
    }
  }

  /**
   * Task 6:
   *
   * Read an XHTML file and print a table of all hyperlinks in the file, together with their URLs.
   * That is, print the child text and the `href` attribute of each a element.
   */
  def printAllHyperlinks(file: String): Unit = {
    val root = XML.load(getClass.getResourceAsStream(file))
    var maxTextLen = 0
    var maxHrefLen = 0

    // extract hyperlinks
    val links = mutable.Buffer[(String, String)]()
    for (n <- root \\ "a";
         hrefAttr <- n.attribute("href")) {

      // extract text from a tag
      val sb = new StringBuilder()
      for (c <- n.child) sb ++= (c match {
        case Text(item) => item.trim
        case item => item.toString()
      })

      val text = sb.toString()
      val href = hrefAttr.text
      maxTextLen = if (maxTextLen < text.length) text.length else maxTextLen
      maxHrefLen = if (maxHrefLen < href.length) href.length else maxHrefLen
      links += Tuple2(text, href)
    }

    val headerAndFooter: String = {
      val sb = new StringBuilder("+")
      for (_ <- 0 until maxTextLen) sb += '-'
      sb ++= "--+--"
      for (_ <- 0 until maxHrefLen) sb += '-'
      sb += '+'
      sb.toString()
    }

    // print extracted hyperlinks as table
    println(headerAndFooter)
    for ((text, href) <- links) {
      print("| ")
      print(text)
      for (_ <- text.length until maxTextLen) print(' ')
      print(" | ")
      print(href)
      for (_ <- href.length until maxHrefLen) print(' ')
      println(" |")
    }

    println(headerAndFooter)
  }

  /**
   * Task 7:
   *
   * Write a function that has a parameter of type `Map[String, String]` and returns a `dl` element
   * with a `dt` for each key and `dd` for each value. For example,
   * {{{
   *  Map("A" -> "1", "B" -> "2")
   * }}}
   * should yield `<dl><dt>A</dt><dd>1</dd><dt>B</dt><dd>2</dd></dl>`.
   */
  def mapToXml(map: Map[String, String]): Elem = {
    <dl>{
      for ((key, value) <- map) yield {
        <dt>{key}</dt>
        <dd>{value}</dd>
      }
    }</dl>
  }
}

object Chapter16PrintImgWithoutAltApp extends Utils.FileApp(Chapter16.printImgWithoutAlt)

object Chapter16PrintAllImgApp extends Utils.FileApp(Chapter16.printAllImg)

object Chapter16PrintAllHyperlinksApp extends Utils.FileApp(Chapter16.printAllHyperlinks)
