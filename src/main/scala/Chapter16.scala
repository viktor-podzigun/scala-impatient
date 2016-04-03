

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
}
