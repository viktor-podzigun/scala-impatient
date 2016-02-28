

object Chapter11 {

  /**
   * Task 1:
   *
   * According to the precedence rules, how are `3 + 4 -> 5` and `3 -> 4 + 5` evaluated?
   *
   * Solution:
   *
   * Since `+` and `-` has the same precedence, then:
   * `3 + 4 -> 5` evaluated as `(3 + 4) -> 5` and
   * `3 -> 4 + 5` evaluated as `(3 -> 4) + 5`
   */

  /**
   * Task 2:
   *
   * The `BigInt` class has a `pow` method, not an operator.
   * Why didn't the Scala library designers choose `**` (as in Fortran) or `^` (as in Pascal)
   * for a power operator?
   *
   * Solution:
   *
   * First, the precedence is determined by the first character, the `**` operator would has
   * the same precedence as `*`, `/` operators, but `pow` operation should have higher precedence.
   * Second, the `^` operator is already exists for `BigInt` class, which is
   * "Bitwise exclusive-or of BigInts".
   */

  /**
   * Task 3:
   *
   * Implement the `Fraction` class with operations `+`, `-`, `*`, `/`. Normalize fractions,
   * for example turning `15/-6` into `-5/2`. Divide by the greatest common divisor, like this:
   * {{{
   *  class Fraction(n: Int, d: Int) {
   *    private val num: Int = if (d == 0) 1 else n * sign(d) / gcd(n, d)
   *    private val den: Int = if (d == 0) 0 else d * sign(d) / gcd(n, d)
   *    override def toString = num + “/” + den
   *    def sign(a: Int) = if (a > 0) 1 else if (a < 0) -1 else 0
   *    def gcd(a: Int, b: Int): Int = if (b == 0) abs(a) else gcd(b, a % b)
   *    ...
   *  }
   * }}}
   */
}
