//// BankingService.scala
//import scala.util.{Try, Success, Failure}
//
//class BankingService {
//  def withdraw(account: Account, amount: Double): Try[Account] = {
//    if (account.balance >= amount) {
//      val updatedAccount = account.copy(balance = account.balance - amount)
//      Success(updatedAccount.copy(transactions = createTransaction(updatedAccount, "Withdraw", amount) :: account.transactions))
//    } else {
//      Failure(new IllegalArgumentException("Insufficient funds"))
//    }
//  }
//
//  def deposit(account: Account, amount: Double): Account = {
//    val updatedAccount = account.copy(balance = account.balance + amount)
//    updatedAccount.copy(transactions = createTransaction(updatedAccount, "Deposit", amount) :: account.transactions)
//  }
//
//  def getAccountInfo(account: Account): String = {
//    s"Account ID: ${account.id}\nBalance: ${account.balance}\nTransaction History: ${account.transactions.reverse.mkString("\n")}"
//  }
//
//  private def createTransaction(account: Account, transactionType: String, amount: Double): Transaction = {
//    Transaction(account.transactions.length + 1, account.id, transactionType, amount)
//  }
//}

// BankingService.scala
import scala.util.{Failure, Success, Try}

class BankingService {
  def deposit(account: Account, amount: Double): Account = {
    val updatedAccount = account.copy(balance = account.balance + amount)
    updatedAccount.copy(transactions = createTransaction(updatedAccount, "Deposit", amount) :: account.transactions)
  }

  def withdraw(account: Account, amount: Double): Try[Account] = {
    if (account.balance >= amount) {
      val updatedAccount = account.copy(balance = account.balance - amount)
      Success(updatedAccount.copy(transactions = createTransaction(updatedAccount, "Withdraw", amount) :: account.transactions))
    } else {
      Failure(new IllegalArgumentException("Insufficient funds"))
    }
  }

  def transfer(fromAccount: Account, toAccount: Account, amount: Double): Try[(Account, Account)] = {
    for {
      withdrawnAccount <- withdraw(fromAccount, amount)
      depositedAccount = deposit(toAccount, amount)
    } yield (withdrawnAccount, depositedAccount)
  }

  def getAccountInfo(account: Account): String = {
    s"Account ID: ${account.id}\nBalance: ${account.balance}"
  }
  def getTransactionDetails(account: Account): String = {
    s"Transaction Details: ${account.transactions.reverse.mkString("\n")}"

  }


  private def createTransaction(account: Account, transactionType: String, amount: Double): Transaction = {
    Transaction(account.transactions.length + 1, account.id, transactionType, amount)
  }
}

