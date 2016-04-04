

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
}
