
/**
 * Task 1:
 *
 * Extend the following `BankAccount` class to a `CheckingAccount` class that charges $1
 * for every deposit and withdrawal.
 * {{{
 * class BankAccount(initialBalance: Double) {
 *   private var balance = initialBalance
 *   def deposit(amount: Double) = { balance += amount; balance }
 *   def withdraw(amount: Double) = { balance -= amount; balance }
 * }
 * }}}
 */
class BankAccount(initialBalance: Double) {
  private var balance = initialBalance

  def deposit(amount: Double) = {
    balance += amount
    balance
  }

  def withdraw(amount: Double) = {
    balance -= amount
    balance
  }
}

class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {

  private val charge: Double = 1

  override def deposit(amount: Double) = super.deposit(amount - charge)

  override def withdraw(amount: Double) = super.withdraw(amount + charge)
}

/**
 * Task 2:
 *
 * Extend the `BankAccount` class of the preceding exercise into a class `SavingsAccount`
 * that earns interest every month (when a method `earnMonthlyInterest` is called)
 * and has three free deposits or withdrawals every month. Reset the transaction
 * count in the `earnMonthlyInterest` method.
 */
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {

  private val monthlyInterest: Double = 0.01
  private val maxFreeTransactions: Int = 3

  private var transactionsCount: Int = 0
  private var balance: Double = initialBalance

  override def deposit(amount: Double) = {
    balance = super.deposit(amount - charge)
    balance
  }

  override def withdraw(amount: Double) = {
    balance = super.withdraw(amount + charge)
    balance
  }

  def getBalance: Double = {
    balance
  }

  def earnMonthlyInterest(): Unit = {
    transactionsCount = 0

    balance = super.deposit(balance * monthlyInterest)
  }

  private def charge: Double = {
    if (transactionsCount < maxFreeTransactions) {
      transactionsCount += 1
      return 0.0
    }

    1.0
  }
}
