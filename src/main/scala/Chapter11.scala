

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
}
