

object Chapter01 {

  /**
   * Task 1:
   *
   * In the Scala REPL, type `3.` followed by the Tab key. What methods can be applied?
   *
   * Solution:
   *
   * On my machine, when typing `3.` and pressing the Tab key for the first time,
   * these methods are available:
   * {{{
   * %   &   *   +   -   /   >   >=   >>   >>>   ^   asInstanceOf   isInstanceOf
   * toByte   toChar   toDouble   toFloat   toInt   toLong   toShort   toString
   * unary_+   unary_-   unary_~   |
   * }}}
   * And when pressing the Tab key the second time:
   * {{{
   * !=   %   *   -   <    <=       ==   >=   >>>   asInstanceOf   getClass   isInstanceOf
   * toChar     toFloat   toLong    toString   unary_-   |
   * ##   &   +   /   <<   <init>   >    >>   ^     equals         hashCode
   * toByte         toDouble   toInt     toShort   unary_+    unary_~
   * }}}
   */

  /**
   * Task 2:
   *
   * In the Scala REPL, compute the square root of 3, and then square that value.
   * By how much does the result differ from 3? (Hint: The res variables are your friend.)
   *
   * Solution:
   *
   * {{{
   * scala> Math.sqrt(3)
   * res0: Double = 1.7320508075688772
   *
   * scala> res0 * res0
   * res1: Double = 2.9999999999999996
   *
   * scala> 3 - res1
   * res2: Double = 4.440892098500626E-16
   * }}}
   */
}
