import scala.xml.XML

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
    for (n <- root \\ "img" if n.attribute("alt").isEmpty;
        src <- n.attribute("src")) {

      println(src.text)
    }
  }
}

object Chapter16PrintImgWithoutAltApp extends Utils.FileApp(Chapter16.printImgWithoutAlt)
