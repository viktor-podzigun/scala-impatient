

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

    def balance: Int = amount
  }

  /**
   * Task 3:
   *  Write a class Time with read-only properties hours and minutes and a method
   *
   *    before(other: Time): Boolean
   *
   *  that checks whether this time comes before the other.
   *
   *  A Time object should be constructed as new Time(hrs, min), where hrs is in
   *  military time format (between 0 and 23).
   */
  class Time03(val hours: Int, val minutes: Int) {

    def before(other: Time03): Boolean = {
      if (hours < other.hours) true
      else if (hours == other.hours && minutes < other.minutes) true
      else false
    }
  }
}
