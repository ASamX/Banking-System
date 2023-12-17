// FileService.scala
import scala.io.Source
import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Using

object FileService {
  //// Method to read customers from CSV file
//  def readCustomers(filePath: String): List[Customer] = {
//    Source.fromFile(filePath).getLines().drop(1).map { line =>
//      val fields = line.split(",")
//      Customer(fields(0).toInt, fields(1), fields(2), fields(3), fields(4), "")
//    }.toList
//  }
  // Method to read customers from CSV file
  def readCustomers(filePath: String): List[Customer] = {
    Using.resource(Source.fromFile(filePath)) { source =>
      source.getLines().drop(1).map { line =>
        val fields = line.split(",")
        Customer(fields(0).toInt, fields(1), fields(2), fields(3), fields(4), "")
      }.toList
    }
  }

  //// Method to read accounts from CSV file
//  def readAccounts(filePath: String): List[Account] = {
//    Source.fromFile(filePath).getLines().drop(1).map { line =>
//      val fields = line.split(",")
//      Account(fields(0).toInt, fields(1).toInt, fields(2).toDouble)
//    }.toList
//  }
  // Method to read accounts from CSV file
  def readAccounts(filePath: String): List[Account] = {
    Using.resource(Source.fromFile(filePath)) { source =>
      source.getLines().drop(1).map { line =>
        val fields = line.split(",")
        Account(fields(0).toInt, fields(1).toInt, fields(2).toDouble)
      }.toList
    }
  }

//  // Method to read transactions from CSV file
//  def readTransactions(filePath: String): List[Transaction] = {
//    Source.fromFile(filePath).getLines().drop(1).map { line =>
//      val fields = line.split(",")
//      Transaction(fields(0).toInt, fields(1).toInt, fields(2), fields(3).toDouble)
//    }.toList
//  }
// Method to read transactions from CSV file
def readTransactions(filePath: String): List[Transaction] = {
  Using.resource(Source.fromFile(filePath)) { source =>
    source.getLines().drop(1).map { line =>
      val fields = line.split(",")
      Transaction(fields(0).toInt, fields(1).toInt, fields(2), fields(3).toDouble)
    }.toList
  }
}
  // Method to write customers to CSV file
  def writeCustomers(filePath: String, customers: List[Customer]): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(filePath)))
    writer.write("id,username,password,name,address\n")
    customers.foreach { customer =>
      writer.write(s"${customer.id},${customer.username},${customer.password},${customer.name},${customer.address}\n")
    }
    writer.close()
  }

  // Method to write accounts to CSV file
  def writeAccounts(filePath: String, accounts: List[Account]): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(filePath)))
    writer.write("id,customerId,balance\n")
    accounts.foreach { account =>
      writer.write(s"${account.id},${account.customerId},${account.balance}\n")
    }
    writer.close()
  }

  // Method to write transactions to CSV file
  def writeTransactions(filePath: String, transactions: List[Transaction]): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(filePath)))
    writer.write("id,accountId,transactionType,amount\n")
    transactions.foreach { transaction =>
      writer.write(s"${transaction.id},${transaction.accountId},${transaction.transactionType},${transaction.amount}\n")
    }
    writer.close()
  }
}
