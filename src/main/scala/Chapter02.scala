

object Chapter02 {

  /**
   * Task 1:
   *
   * The `signum` of a number is `1` if the number is positive, `–1` if it is negative,
   * and `0` if it is zero. Write a function that computes this value.
   */
  def signum(n: Int): Int = {
    if (n < 0) -1
    else if (n > 0) 1
    else 0
  }
}
