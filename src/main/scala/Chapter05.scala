

object Chapter05 {

  /**
   * Task 1:
   *  Improve the Counter class in Section 5.1, "Simple Classes and Parameterless Methods"
   *  on page 51 so that it doesn't turn negative at Int.MaxValue
   */
  class Counter01(private var value: Int = 0) {

    def increment(): Int = {
      if (value == Int.MaxValue) {
        throw new IllegalStateException("counter reached Int.MaxValue")
      }

      value += 1
      value
    }

    def current: Int = value
  }

  /**
   * Task 2:
   *  Write a class BankAccount with methods deposit and withdraw,
   *  and a read-only property balance.
   */
  class BankAccount02 {

    private var amount: Int = 0

    def deposit(sum: Int): Unit = {
      amount += sum
    }

    def withdraw(sum: Int): Unit = {
      amount -= sum
    }

    def balance: Int = {
      amount
    }
  }
}
