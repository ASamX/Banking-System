import akka.actor.ActorSystem

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object Main extends App {
  // Create an Actor System
  private val actorSystem = ActorSystem("BankingSystem")

  // Example: Read data from CSV files
  var customers = FileService.readCustomers("src/main/scala/customers.csv")
  var accounts = FileService.readAccounts("src/main/scala/accounts.csv")
  var transactions = FileService.readTransactions("src/main/scala/transactions.csv")

  private def editAccount(customer: Customer, customers: List[Customer]): List[Customer] = {
    println("Choose what you want to edit:")
    println("1. Name")
    println("2. Username")
    println("3. Password")
    println("4. Address")
    print("Enter the number of your choice: ")

    val editChoice = StdIn.readInt()

    val updatedCustomer = editChoice match {
      case 1 =>
        // Edit name
        println("Enter your new name:")
        val newName = StdIn.readLine()
        customer.copy(name = newName)

      case 2 =>
        // Edit username
        println("Enter your new username:")
        val newUsername = StdIn.readLine()
        customer.copy(username = newUsername)

      case 3 =>
        // Edit password
        println("Enter your new password:")
        val newPassword = StdIn.readLine()
        customer.copy(password = newPassword)

      case 4 =>
        // Edit address
        println("Enter your new address:")
        val newAddress = StdIn.readLine()
        customer.copy(address = newAddress)

      case _ =>
        println("Invalid choice. No changes made.")
        customer
    }

    // Update the customers list with the edited customer information
    customers.map(c => if (c.id == customer.id) updatedCustomer else c)
  }

  private def deleteAccount(customer: Customer, customers: List[Customer], accounts: List[Account], transactions: List[Transaction]): (List[Customer], List[Account], List[Transaction]) = {
    println("Are you sure you want to delete your account? (Y/N)")
    val confirmation = StdIn.readLine()

    if (confirmation.equalsIgnoreCase("Y")) {
      // Remove the customer from the customers list
      val updatedCustomers = customers.filterNot(_.id == customer.id)

      // Remove the customer's account from the accounts list
      val updatedAccounts = accounts.filterNot(_.customerId == customer.id)

      // Remove the customer's transactions from the transactions list
      val updatedTransactions = transactions.filterNot(_.accountId == customer.id)

      println("Account deletion successful.")
      (updatedCustomers, updatedAccounts, updatedTransactions)
    } else {
      println("Account deletion cancelled.")
      (customers, accounts, transactions)
    }
  }


  private def viewAccountDetails(customer: Customer, userAccount: Account): Unit = {
    println("Account Details:")
    println(s"Customer ID: ${customer.id}")
    println(s"Name: ${customer.name}")
    println(s"Username: ${customer.username}")
    println(s"Address: ${customer.address}")
    println("\nAccount Information:")
    println(s"Account ID: ${userAccount.id}")
    println(s"Balance: ${userAccount.balance}")
  }

  private def viewTransactionDetails(userAccount: Account): Unit = {
    val userTransactions = transactions.filter(_.accountId == userAccount.id)
    println("\nTransaction History:")
    userTransactions.foreach { transaction =>
      println(s"Transaction ID: ${transaction.id}, Type: ${transaction.transactionType}, Amount: ${transaction.amount}")
    }
  }

  private def accountDetailsMenu(customer: Customer, userAccount: Account): Unit = {
    var exitAccountDetails = false
    while (!exitAccountDetails) {
      println("\nAccount Details Menu:")
      println("1. View Account Details")
      println("2. View Transaction History")
      println("3. Exit")
      print("Enter the number of your choice: ")

      val accountDetailsChoice = StdIn.readInt()

      accountDetailsChoice match {
        case 1 =>
          viewAccountDetails(customer, userAccount)

        case 2 =>
          val filteredTransactions = transactions.filter(_.accountId == userAccount.id)
          println(s"Number of Transactions Found: ${filteredTransactions.size}")
          viewTransactionDetails(userAccount)

        case 3 =>
          exitAccountDetails = true

        case _ =>
          println("Invalid choice. Please try again.")
      }
    }
  }


  // Ask the user if they want to log in or create a new account
  println("Choose an option:")
  println("1. Log in")
  println("2. Create a new account")
  print("Enter the number of your choice: ")

  private val optionChoice = StdIn.readInt()

  optionChoice match {
    case 1 =>
      // User chose to log in
      println("Enter your username:")
      val username = StdIn.readLine()
      println("Enter your password:")
      val password = StdIn.readLine()

      AuthService.login(username, password, customers) match {

        case Some(customer) =>
          println(s"Login successful. Welcome, ${customer.name}!")

          // Retrieve user account
          val userAccount = accounts.find(_.customerId == customer.id).getOrElse(Account(0, 0, 0.0))

          // Perform banking operations
          val bankingService = new BankingService()

          // Ask the user for the desired action
          println("Choose an action:")
          println("1. Deposit")
          println("2. Withdraw")
          println("3. Transfer")
          println("4. Edit Account")
          println("5. Delete Account")
          println("6. Account Details")
          print("Enter the number of your choice: ")

          // Read the user's choice
          val choice = StdIn.readInt()

          // Perform the chosen action
          choice match {
            case 1 =>
              // Deposit
              print("Enter the amount to deposit: ")
              val depositAmount = StdIn.readDouble()
              val updatedAccount = bankingService.deposit(userAccount, depositAmount)
              println(s"Deposit successful. Updated Account Information:")
              println(bankingService.getAccountInfo(updatedAccount))
              println(bankingService.getTransactionDetails(updatedAccount))

              // Update the 'accounts' list with the new account information
              accounts = accounts.map(a => if (a.customerId == userAccount.customerId) updatedAccount else a)

              // Write the updated account information back to the CSV file
              FileService.writeAccounts("src/main/scala/accounts.csv", accounts)

              // Create and record the deposit transaction
              val depositTransaction = Transaction(id = transactions.length + 1, accountId = userAccount.id, transactionType = "Deposit", amount = depositAmount)
              transactions = transactions :+ depositTransaction

              // Write the updated transactions back to the CSV file
              FileService.writeTransactions("src/main/scala/transactions.csv", transactions)

            case 2 =>
              // Withdraw
              print("Enter the amount to withdraw: ")
              val withdrawAmount = StdIn.readDouble()
              val withdrawalResult: Try[Account] = bankingService.withdraw(userAccount, withdrawAmount)
              withdrawalResult match {
                case Success(withdrawnAccount) =>
                  println("Withdrawal successful. Updated Account Information:")
                  println(bankingService.getAccountInfo(withdrawnAccount))
                  println(bankingService.getTransactionDetails(withdrawnAccount))

                  // Update the 'accounts' list with the new account information
                  accounts = accounts.map(a => if (a.customerId == userAccount.customerId) withdrawnAccount else a)

                  // Write the updated account information back to the CSV file
                  FileService.writeAccounts("src/main/scala/accounts.csv", accounts)

                  // Create and record the withdrawal transaction
                  val withdrawalTransaction = Transaction(id = transactions.length + 1, accountId = userAccount.id, transactionType = "Withdrawal", amount = withdrawAmount)
                  transactions = transactions :+ withdrawalTransaction

                  // Write the updated transactions back to the CSV file
                  FileService.writeTransactions("src/main/scala/transactions.csv", transactions)

                case Failure(exception) =>
                  println(s"Error: ${exception.getMessage}")
              }

            case 3 =>
              // Transfer
              print("Enter the username of the recipient: ")
              val recipientUsername = StdIn.readLine()
              val recipient = customers.find(_.username == recipientUsername)

              recipient match {
                case Some(recipientCustomer) =>
                  print("Enter the amount to transfer: ")
                  val transferAmount = StdIn.readDouble()
                  val transferResult: Try[(Account, Account)] = bankingService.transfer(userAccount, accounts.find(_.customerId == recipientCustomer.id).getOrElse(Account(0, 0, 0.0)), transferAmount)

                  transferResult match {
                    case Success((senderAccount, recipientAccount)) =>
                      println("Transfer successful.")
                      println(s"Sender's Updated Account Information:")
                      println(bankingService.getAccountInfo(senderAccount))
                      println(bankingService.getTransactionDetails(senderAccount))
                      println(s"Recipient's Updated Account Information:")
                      println(bankingService.getAccountInfo(recipientAccount))
                      println(bankingService.getTransactionDetails(recipientAccount))

                      // Update the 'accounts' list with the new account information
                      accounts = accounts.map(a => if (a.customerId == userAccount.customerId) senderAccount else if (a.customerId == recipientCustomer.id) recipientAccount else a)

                      // Write the updated account information back to the CSV file
                      FileService.writeAccounts("src/main/scala/accounts.csv", accounts)

                      // Create and record the transfer transactions
                      val senderTransaction = Transaction(id = transactions.length + 1, accountId = senderAccount.id, transactionType = "Transfer (Sent)", amount = transferAmount, otherParty = recipientCustomer.name)
                      val recipientTransaction = Transaction(id = transactions.length + 2, accountId = recipientAccount.id, transactionType = "Transfer (Received)", amount = transferAmount, otherParty = senderAccount.customerId.toString)
                      transactions = transactions :+ senderTransaction :+ recipientTransaction

                      // Write the updated transactions back to the CSV file
                      FileService.writeTransactions("src/main/scala/transactions.csv", transactions)

                    case Failure(exception) =>
                      println(s"Error: ${exception.getMessage}")
                  }

                case None =>
                  println("Recipient not found.")
              }

            case 4 =>
              // Edit Account
              customers = editAccount(customer, customers)
              FileService.writeCustomers("src/main/scala/customers.csv", customers)
              println("Account updated successfully.")

            case 5 =>
              // Delete Account
              val (updatedCustomers, updatedAccounts, updatedTransactions) = deleteAccount(customer, customers, accounts, transactions)
              FileService.writeCustomers("src/main/scala/customers.csv", updatedCustomers)
              FileService.writeAccounts("src/main/scala/accounts.csv", updatedAccounts)
              FileService.writeTransactions("src/main/scala/transactions.csv", updatedTransactions)
              println("Account deleted successfully. Exiting.")
              System.exit(0)

            case 6 =>
              // Account Details
              accountDetailsMenu(customer, userAccount)

            case _ =>
              println("Invalid choice. Exiting.")
          }

        case None =>
          println("Login failed. Invalid username or password.")
      }

    case 2 =>
      // User chose to create a new account
      println("Enter your full name:")
      val name = StdIn.readLine()
      println("Enter your desired username:")
      val newUsername = StdIn.readLine()
      println("Enter your desired password:")
      val newPassword = StdIn.readLine()
      println("Enter your current address:")
      val address = StdIn.readLine()

      customers = AuthService.createAccount(name, newUsername, newPassword, address, customers)

      // Optionally, update the CSV file with the new customer data
      FileService.writeCustomers("src/main/scala/customers.csv", customers)

      println("Account created successfully. You can now log in.")


    case _ =>
      println("Invalid choice. Exiting.")
  }

  // Shutdown the Actor System
  actorSystem.terminate()
}

///////////////////////////////////////////////////////////////////// Second
////import akka.actor.ActorSystem
////
////import scala.io.StdIn
////import scala.util.{Failure, Success, Try}
////
////object Main extends App {
////  // Create an Actor System
////  private val actorSystem = ActorSystem("BankingSystem")
////
////  // Example: Read data from CSV files
////  var customers = FileService.readCustomers("src/main/scala/customers.csv")
////  var accounts = FileService.readAccounts("src/main/scala/accounts.csv")
////  var transactions = FileService.readTransactions("src/main/scala/transactions.csv")
////
////  private def editAccount(customer: Customer, customers: List[Customer]): List[Customer] = {
////    println("Choose what you want to edit:")
////    println("1. Name")
////    println("2. Username")
////    println("3. Password")
////    println("4. Address")
////    print("Enter the number of your choice: ")
////
////    val editChoice = StdIn.readInt()
////
////    val updatedCustomer = editChoice match {
////      case 1 =>
////        // Edit name
////        println("Enter your new name:")
////        val newName = StdIn.readLine()
////        customer.copy(name = newName)
////
////      case 2 =>
////        // Edit username
////        println("Enter your new username:")
////        val newUsername = StdIn.readLine()
////        customer.copy(username = newUsername)
////
////      case 3 =>
////        // Edit password
////        println("Enter your new password:")
////        val newPassword = StdIn.readLine()
////        customer.copy(password = newPassword)
////
////      case 4 =>
////        // Edit address
////        println("Enter your new address:")
////        val newAddress = StdIn.readLine()
////        customer.copy(address = newAddress)
////
////      case _ =>
////        println("Invalid choice. No changes made.")
////        customer
////    }
////
////    // Update the customers list with the edited customer information
////    customers.map(c => if (c.id == customer.id) updatedCustomer else c)
////  }
////
////  private def deleteAccount(customer: Customer, customers: List[Customer], accounts: List[Account], transactions: List[Transaction]): (List[Customer], List[Account], List[Transaction]) = {
////    println("Are you sure you want to delete your account? (Y/N)")
////    val confirmation = StdIn.readLine()
////
////    if (confirmation.equalsIgnoreCase("Y")) {
////      // Remove the customer from the customers list
////      val updatedCustomers = customers.filterNot(_.id == customer.id)
////
////      // Remove the customer's account from the accounts list
////      val updatedAccounts = accounts.filterNot(_.customerId == customer.id)
////
////      // Remove the customer's transactions from the transactions list
////      val updatedTransactions = transactions.filterNot(_.accountId == customer.id)
////
////      println("Account deletion successful.")
////      (updatedCustomers, updatedAccounts, updatedTransactions)
////    } else {
////      println("Account deletion cancelled.")
////      (customers, accounts, transactions)
////    }
////  }
////
////  private def viewAccountDetails(customer: Customer, userAccount: Account): Unit = {
////    println("Account Details:")
////    println(s"Customer ID: ${customer.id}")
////    println(s"Name: ${customer.name}")
////    println(s"Username: ${customer.username}")
////    println(s"Address: ${customer.address}")
////    println("\nAccount Information:")
////    println(s"Account ID: ${userAccount.id}")
////    println(s"Balance: ${userAccount.balance}")
////  }
////
////  private def viewTransactionDetails(userAccount: Account): Unit = {
////    val userTransactions = transactions.filter(_.accountId == userAccount.id)
////    println("\nTransaction History:")
////    userTransactions.foreach { transaction =>
////      println(s"Transaction ID: ${transaction.id}, Type: ${transaction.transactionType}, Amount: ${transaction.amount}")
////    }
////  }
////
////  private def accountDetailsMenu(customer: Customer, userAccount: Account): Unit = {
////    val exitAccountDetails = false
////    while (!exitAccountDetails) {
////      println("\nAccount Details Menu:")
////      println("1. View Account Details")
////      println("2. View Transaction History")
////      println("3. Back to Banking Menu")  // Changed the option label
////      print("Enter the number of your choice: ")
////
////      val accountDetailsChoice = StdIn.readInt()
////
////      accountDetailsChoice match {
////        case 1 =>
////          viewAccountDetails(customer, userAccount)
////
////        case 2 =>
////          val filteredTransactions = transactions.filter(_.accountId == userAccount.id)
////          println(s"Number of Transactions Found: ${filteredTransactions.size}")
////          viewTransactionDetails(userAccount)
////
////        case 3 =>
////          val userAccount = accounts.find(_.customerId == customer.id).getOrElse(Account(0, 0, 0.0))
////
////          // Perform banking operations
////          val bankingService = new BankingService()
////          println("Choose an action:")
////          println("1. Deposit")
////          println("2. Withdraw")
////          println("3. Transfer")
////          println("4. Edit Account")
////          println("5. Delete Account")
////          println("6. Account Details")
////          print("Enter the number of your choice: ")
////
////          // Read the user's choice
////          val choice = StdIn.readInt()
////
////          // Perform the chosen action
////          choice match {
////            case 1 =>
////              // Deposit
////              print("Enter the amount to deposit: ")
////              val depositAmount = StdIn.readDouble()
////              val updatedAccount = bankingService.deposit(userAccount, depositAmount)
////              println(s"Deposit successful. Updated Account Information:")
////              println(bankingService.getAccountInfo(updatedAccount))
////              println(bankingService.getTransactionDetails(updatedAccount))
////
////              // Update the 'accounts' list with the new account information
////              accounts = accounts.map(a => if (a.customerId == userAccount.customerId) updatedAccount else a)
////
////              // Write the updated account information back to the CSV file
////              FileService.writeAccounts("src/main/scala/accounts.csv", accounts)
////
////              // Create and record the deposit transaction
////              val depositTransaction = Transaction(id = transactions.length + 1, accountId = userAccount.id, transactionType = "Deposit", amount = depositAmount)
////              transactions = transactions :+ depositTransaction
////
////              // Write the updated transactions back to the CSV file
////              FileService.writeTransactions("src/main/scala/transactions.csv", transactions)
////              System.exit(0)
////            case 2 =>
////              // Withdraw
////              print("Enter the amount to withdraw: ")
////              val withdrawAmount = StdIn.readDouble()
////              val withdrawalResult: Try[Account] = bankingService.withdraw(userAccount, withdrawAmount)
////              withdrawalResult match {
////                case Success(withdrawnAccount) =>
////                  println("Withdrawal successful. Updated Account Information:")
////                  println(bankingService.getAccountInfo(withdrawnAccount))
////                  println(bankingService.getTransactionDetails(withdrawnAccount))
////
////                  // Update the 'accounts' list with the new account information
////                  accounts = accounts.map(a => if (a.customerId == userAccount.customerId) withdrawnAccount else a)
////
////                  // Write the updated account information back to the CSV file
////                  FileService.writeAccounts("src/main/scala/accounts.csv", accounts)
////
////                  // Create and record the withdrawal transaction
////                  val withdrawalTransaction = Transaction(id = transactions.length + 1, accountId = userAccount.id, transactionType = "Withdrawal", amount = withdrawAmount)
////                  transactions = transactions :+ withdrawalTransaction
////
////                  // Write the updated transactions back to the CSV file
////                  FileService.writeTransactions("src/main/scala/transactions.csv", transactions)
////
////                case Failure(exception) =>
////                  println(s"Error: ${exception.getMessage}")
////              }
////              System.exit(0)
////            case 3 =>
////              // Transfer
////              print("Enter the username of the recipient: ")
////              val recipientUsername = StdIn.readLine()
////              val recipient = customers.find(_.username == recipientUsername)
////
////              recipient match {
////                case Some(recipientCustomer) =>
////                  print("Enter the amount to transfer: ")
////                  val transferAmount = StdIn.readDouble()
////                  val transferResult: Try[(Account, Account)] = bankingService.transfer(userAccount, accounts.find(_.customerId == recipientCustomer.id).getOrElse(Account(0, 0, 0.0)), transferAmount)
////
////                  transferResult match {
////                    case Success((senderAccount, recipientAccount)) =>
////                      println("Transfer successful.")
////                      println(s"Sender's Updated Account Information:")
////                      println(bankingService.getAccountInfo(senderAccount))
////                      println(bankingService.getTransactionDetails(senderAccount))
////                      println(s"Recipient's Updated Account Information:")
////                      println(bankingService.getAccountInfo(recipientAccount))
////                      println(bankingService.getTransactionDetails(recipientAccount))
////
////                      // Update the 'accounts' list with the new account information
////                      accounts = accounts.map(a => if (a.customerId == userAccount.customerId) senderAccount else if (a.customerId == recipientCustomer.id) recipientAccount else a)
////
////                      // Write the updated account information back to the CSV file
////                      FileService.writeAccounts("src/main/scala/accounts.csv", accounts)
////
////                      // Create and record the transfer transactions
////                      val senderTransaction = Transaction(id = transactions.length + 1, accountId = senderAccount.id, transactionType = "Transfer (Sent)", amount = transferAmount, otherParty = recipientCustomer.name)
////                      val recipientTransaction = Transaction(id = transactions.length + 2, accountId = recipientAccount.id, transactionType = "Transfer (Received)", amount = transferAmount, otherParty = senderAccount.customerId.toString)
////                      transactions = transactions :+ senderTransaction :+ recipientTransaction
////
////                      // Write the updated transactions back to the CSV file
////                      FileService.writeTransactions("src/main/scala/transactions.csv", transactions)
////
////                    case Failure(exception) =>
////                      println(s"Error: ${exception.getMessage}")
////                  }
////                case None =>
////                  println("Recipient not found.")
////              }
////              System.exit(0)
////            case 4 =>
////              // Edit Account
////              customers = editAccount(customer, customers)
////              FileService.writeCustomers("src/main/scala/customers.csv", customers)
////              println("Account updated successfully.")
////
////            case 5 =>
////              // Delete Account
////              val (updatedCustomers, updatedAccounts, updatedTransactions) = deleteAccount(customer, customers, accounts, transactions)
////              FileService.writeCustomers("src/main/scala/customers.csv", updatedCustomers)
////              FileService.writeAccounts("src/main/scala/accounts.csv", updatedAccounts)
////              FileService.writeTransactions("src/main/scala/transactions.csv", updatedTransactions)
////              println("Account deleted successfully. Exiting.")
////              System.exit(0)
////
////            case 6 =>
////              // Account Details
////              accountDetailsMenu(customer, userAccount)
////
////            case _ =>
////              println("Invalid choice. Exiting.")
////          }
////
////        case _ =>
////          println("Invalid choice. Please try again.")
////      }
////    }
////  }
////
////  // Ask the user if they want to log in or create a new account
////  println("Choose an option:")
////  println("1. Log in")
////  println("2. Create a new account")
////  print("Enter the number of your choice: ")
////
////  private val optionChoice = StdIn.readInt()
////
////  optionChoice match {
////    case 1 =>
////      // User chose to log in
////      println("Enter your username:")
////      val username = StdIn.readLine()
////      println("Enter your password:")
////      val password = StdIn.readLine()
////
////      AuthService.login(username, password, customers) match {
////
////        case Some(customer) =>
////          println(s"Login successful. Welcome, ${customer.name}!")
////
////          // Retrieve user account
////          val userAccount = accounts.find(_.customerId == customer.id).getOrElse(Account(0, 0, 0.0))
////
////          // Perform banking operations
////          val bankingService = new BankingService()
////
////          // Ask the user for the desired action
////          println("Choose an action:")
////          println("1. Deposit")
////          println("2. Withdraw")
////          println("3. Transfer")
////          println("4. Edit Account")
////          println("5. Delete Account")
////          println("6. Account Details")
////          print("Enter the number of your choice: ")
////
////          // Read the user's choice
////          val choice = StdIn.readInt()
////
////          // Perform the chosen action
////          choice match {
////            case 1 =>
////                          // Deposit
////                          print("Enter the amount to deposit: ")
////                          val depositAmount = StdIn.readDouble()
////                          val updatedAccount = bankingService.deposit(userAccount, depositAmount)
////                          println(s"Deposit successful. Updated Account Information:")
////                          println(bankingService.getAccountInfo(updatedAccount))
////                          println(bankingService.getTransactionDetails(updatedAccount))
////
////                          // Update the 'accounts' list with the new account information
////                          accounts = accounts.map(a => if (a.customerId == userAccount.customerId) updatedAccount else a)
////
////                          // Write the updated account information back to the CSV file
////                          FileService.writeAccounts("src/main/scala/accounts.csv", accounts)
////
////                          // Create and record the deposit transaction
////                          val depositTransaction = Transaction(id = transactions.length + 1, accountId = userAccount.id, transactionType = "Deposit", amount = depositAmount)
////                          transactions = transactions :+ depositTransaction
////
////                          // Write the updated transactions back to the CSV file
////                          FileService.writeTransactions("src/main/scala/transactions.csv", transactions)
////
////                        case 2 =>
////                          // Withdraw
////                          print("Enter the amount to withdraw: ")
////                          val withdrawAmount = StdIn.readDouble()
////                          val withdrawalResult: Try[Account] = bankingService.withdraw(userAccount, withdrawAmount)
////                          withdrawalResult match {
////                            case Success(withdrawnAccount) =>
////                              println("Withdrawal successful. Updated Account Information:")
////                              println(bankingService.getAccountInfo(withdrawnAccount))
////                              println(bankingService.getTransactionDetails(withdrawnAccount))
////
////                              // Update the 'accounts' list with the new account information
////                              accounts = accounts.map(a => if (a.customerId == userAccount.customerId) withdrawnAccount else a)
////
////                              // Write the updated account information back to the CSV file
////                              FileService.writeAccounts("src/main/scala/accounts.csv", accounts)
////
////                              // Create and record the withdrawal transaction
////                              val withdrawalTransaction = Transaction(id = transactions.length + 1, accountId = userAccount.id, transactionType = "Withdrawal", amount = withdrawAmount)
////                              transactions = transactions :+ withdrawalTransaction
////
////                              // Write the updated transactions back to the CSV file
////                              FileService.writeTransactions("src/main/scala/transactions.csv", transactions)
////
////                            case Failure(exception) =>
////                              println(s"Error: ${exception.getMessage}")
////                          }
////
////                        case 3 =>
////                          // Transfer
////                          print("Enter the username of the recipient: ")
////                          val recipientUsername = StdIn.readLine()
////                          val recipient = customers.find(_.username == recipientUsername)
////
////                          recipient match {
////                            case Some(recipientCustomer) =>
////                              print("Enter the amount to transfer: ")
////                              val transferAmount = StdIn.readDouble()
////                              val transferResult: Try[(Account, Account)] = bankingService.transfer(userAccount, accounts.find(_.customerId == recipientCustomer.id).getOrElse(Account(0, 0, 0.0)), transferAmount)
////
////                              transferResult match {
////                                case Success((senderAccount, recipientAccount)) =>
////                                  println("Transfer successful.")
////                                  println(s"Sender's Updated Account Information:")
////                                  println(bankingService.getAccountInfo(senderAccount))
////                                  println(bankingService.getTransactionDetails(senderAccount))
////                                  println(s"Recipient's Updated Account Information:")
////                                  println(bankingService.getAccountInfo(recipientAccount))
////                                  println(bankingService.getTransactionDetails(recipientAccount))
////
////                                  // Update the 'accounts' list with the new account information
////                                  accounts = accounts.map(a => if (a.customerId == userAccount.customerId) senderAccount else if (a.customerId == recipientCustomer.id) recipientAccount else a)
////
////                                  // Write the updated account information back to the CSV file
////                                  FileService.writeAccounts("src/main/scala/accounts.csv", accounts)
////
////                                  // Create and record the transfer transactions
////                                  val senderTransaction = Transaction(id = transactions.length + 1, accountId = senderAccount.id, transactionType = "Transfer (Sent)", amount = transferAmount, otherParty = recipientCustomer.name)
////                                  val recipientTransaction = Transaction(id = transactions.length + 2, accountId = recipientAccount.id, transactionType = "Transfer (Received)", amount = transferAmount, otherParty = senderAccount.customerId.toString)
////                                  transactions = transactions :+ senderTransaction :+ recipientTransaction
////
////                                  // Write the updated transactions back to the CSV file
////                                  FileService.writeTransactions("src/main/scala/transactions.csv", transactions)
////
////                                case Failure(exception) =>
////                                  println(s"Error: ${exception.getMessage}")
////                              }
////                            case None =>
////                                              println("Recipient not found.")
////                                          }
////
////            case 4 =>
////              // Edit Account
////              customers = editAccount(customer, customers)
////              FileService.writeCustomers("src/main/scala/customers.csv", customers)
////              println("Account updated successfully.")
////
////            case 5 =>
////              // Delete Account
////              val (updatedCustomers, updatedAccounts, updatedTransactions) = deleteAccount(customer, customers, accounts, transactions)
////              FileService.writeCustomers("src/main/scala/customers.csv", updatedCustomers)
////              FileService.writeAccounts("src/main/scala/accounts.csv", updatedAccounts)
////              FileService.writeTransactions("src/main/scala/transactions.csv", updatedTransactions)
////              println("Account deleted successfully. Exiting.")
////              System.exit(0)
////
////            case 6 =>
////              // Account Details
////              accountDetailsMenu(customer, userAccount)
////
////            case _ =>
////              println("Invalid choice. Exiting.")
////          }
////
////        case None =>
////          println("Login failed. Invalid username or password.")
////      }
////
////    case 2 =>
////      // User chose to create a new account
////      println("Enter your full name:")
////      val name = StdIn.readLine()
////      println("Enter your desired username:")
////      val newUsername = StdIn.readLine()
////      println("Enter your desired password:")
////      val newPassword = StdIn.readLine()
////      println("Enter your current address:")
////      val address = StdIn.readLine()
////
////      customers = AuthService.createAccount(name, newUsername, newPassword, address, customers)
////
////      // Optionally, update the CSV file with the new customer data
////      FileService.writeCustomers("src/main/scala/customers.csv", customers)
////
////      println("Account created successfully. You can now log in.")
////
////    case _ =>
////      println("Invalid choice. Exiting.")
////  }
////
////  // Shutdown the Actor System
////  actorSystem.terminate()
////}
/////////////////////////////////////////////////////////////////////////////////////////// Third
////import akka.actor.ActorSystem
////
////import scala.io.StdIn
////import scala.util.{Failure, Success}
////
////object Main extends App {
////  private val actorSystem = ActorSystem("BankingSystem")
////
////  var customers = FileService.readCustomers("src/main/scala/customers.csv")
////  var accounts = FileService.readAccounts("src/main/scala/accounts.csv")
////  var transactions = FileService.readTransactions("src/main/scala/transactions.csv")
////  private val bankingService = new BankingService()
////
////  private def editAccount(customer: Customer, customers: List[Customer]): List[Customer] = {
////    println("Choose what you want to edit:")
////    println("1. Name")
////    println("2. Username")
////    println("3. Password")
////    println("4. Address")
////    print("Enter the number of your choice: ")
////
////    val editChoice = StdIn.readInt()
////
////    val updatedCustomer = editChoice match {
////      case 1 =>
////        // Edit name
////        println("Enter your new name:")
////        val newName = StdIn.readLine()
////        customer.copy(name = newName)
////
////      case 2 =>
////        // Edit username
////        println("Enter your new username:")
////        val newUsername = StdIn.readLine()
////        customer.copy(username = newUsername)
////
////      case 3 =>
////        // Edit password
////        println("Enter your new password:")
////        val newPassword = StdIn.readLine()
////        customer.copy(password = newPassword)
////
////      case 4 =>
////        // Edit address
////        println("Enter your new address:")
////        val newAddress = StdIn.readLine()
////        customer.copy(address = newAddress)
////
////      case _ =>
////        println("Invalid choice. No changes made.")
////        customer
////    }
////
////    // Update the customers list with the edited customer information
////    customers.map(c => if (c.id == customer.id) updatedCustomer else c)
////  }
////
////
////  private def editAccountOption(customer: Customer): Unit = {
////    customers = editAccount(customer, customers)
////    FileService.writeCustomers("src/main/scala/customers.csv", customers)
////    println("Account updated successfully.")
////  }
////
////  private def deleteAccount(customer: Customer, customers: List[Customer], accounts: List[Account], transactions: List[Transaction]): (List[Customer], List[Account], List[Transaction]) = {
////    println("Are you sure you want to delete your account? (Y/N)")
////    val confirmation = StdIn.readLine()
////
////    if (confirmation.equalsIgnoreCase("Y")) {
////      // Remove the customer from the customers list
////      val updatedCustomers = customers.filterNot(_.id == customer.id)
////
////      // Remove the customer's account from the accounts list
////      val updatedAccounts = accounts.filterNot(_.customerId == customer.id)
////
////      // Remove the customer's transactions from the transactions list
////      val updatedTransactions = transactions.filterNot(_.accountId == customer.id)
////
////      println("Account deletion successful.")
////      (updatedCustomers, updatedAccounts, updatedTransactions)
////    } else {
////      println("Account deletion cancelled.")
////      (customers, accounts, transactions)
////    }
////  }
////
////
////  private def deleteAccountOption(customer: Customer): Unit = {
////    val (updatedCustomers, updatedAccounts, updatedTransactions) = deleteAccount(customer, customers, accounts, transactions)
////    FileService.writeCustomers("src/main/scala/customers.csv", updatedCustomers)
////    FileService.writeAccounts("src/main/scala/accounts.csv", updatedAccounts)
////    FileService.writeTransactions("src/main/scala/transactions.csv", updatedTransactions)
////    println("Account deleted successfully. Exiting.")
////    System.exit(0)
////  }
////
////  private def viewAccountDetails(customer: Customer, userAccount: Account): Unit = {
////    println("Account Details:")
////    println(s"Customer ID: ${customer.id}")
////    println(s"Name: ${customer.name}")
////    println(s"Username: ${customer.username}")
////    println(s"Address: ${customer.address}")
////    println("\nAccount Information:")
////    println(s"Account ID: ${userAccount.id}")
////    println(s"Balance: ${userAccount.balance}")
////  }
////
////  private def viewTransactionDetails(userAccount: Account): Unit = {
////    val userTransactions = transactions.filter(_.accountId == userAccount.id)
////    println("\nTransaction History:")
////    userTransactions.foreach { transaction =>
////      println(s"Transaction ID: ${transaction.id}, Type: ${transaction.transactionType}, Amount: ${transaction.amount}")
////    }
////  }
////
////
////
////  private def accountDetailsOption(customer: Customer, userAccount: Account): Unit = {
////    val exitAccountDetails = false
////    while (!exitAccountDetails) {
////      println("\nAccount Details Menu:")
////      println("1. View Account Details")
////      println("2. View Transaction History")
////      println("3. Back to Banking Menu")
////      print("Enter the number of your choice: ")
////
////      val accountDetailsChoice = StdIn.readInt()
////
////      accountDetailsChoice match {
////        case 1 =>
////          viewAccountDetails(customer, userAccount)
////
////        case 2 =>
////          viewTransactionDetails(userAccount)
////
////        case 3 =>
////          handleLoggedInCustomer(customer)
////
////        case _ =>
////          println("Invalid choice. Please try again.")
////      }
////    }
////  }
////
////
////  private def handleLoggedInCustomer(customer: Customer): Unit = {
////    println(s"Login successful. Welcome, ${customer.name}!")
////
////    val userAccount = accounts.find(_.customerId == customer.id).getOrElse(Account(0, 0, 0.0))
////
////    println("Choose an action:")
////    println("1. Deposit")
////    println("2. Withdraw")
////    println("3. Transfer")
////    println("4. Edit Account")
////    println("5. Delete Account")
////    println("6. Account Details")
////    print("Enter the number of your choice: ")
////
////    val choice = StdIn.readInt()
////
////    choice match {
////      case 1 => depositOption(userAccount)
////      case 2 => withdrawOption(userAccount)
////      case 3 => transferOption(userAccount)
////      case 4 => editAccountOption(customer)
////      case 5 => deleteAccountOption(customer)
////      case 6 => accountDetailsOption(customer, userAccount)
////      case _ => println("Invalid choice. Exiting.")
////    }
////  }
////
////  private def depositOption(userAccount: Account): Unit = {
////    print("Enter the amount to deposit: ")
////    val depositAmount = StdIn.readDouble()
////
////    val updatedAccount = bankingService.deposit(userAccount, depositAmount)
////    updateAccountInformation(updatedAccount)
////    println("Exiting.")
////    System.exit(0)
////  }
////
////  private def withdrawOption(userAccount: Account): Unit = {
////    print("Enter the amount to withdraw: ")
////    val withdrawAmount = StdIn.readDouble()
////
////    bankingService.withdraw(userAccount, withdrawAmount) match {
////      case Success(updatedAccount) => updateAccountInformation(updatedAccount)
////        println("Exiting.")
////        System.exit(0)
////      case Failure(exception) => println(s"Error: ${exception.getMessage}")
////    }
////  }
////
////  private def transferOption(userAccount: Account): Unit = {
////    print("Enter the username of the recipient: ")
////    val recipientUsername = StdIn.readLine()
////    val recipient = customers.find(_.username == recipientUsername)
////
////    recipient match {
////      case Some(recipientCustomer) => performTransfer(userAccount, recipientCustomer)
////      case None => println("Recipient not found.")
////    }
////  }
////
////  private def performTransfer(userAccount: Account, recipientCustomer: Customer): Unit = {
////    print("Enter the amount to transfer: ")
////    val transferAmount = StdIn.readDouble()
////
////    bankingService.transfer(userAccount, accounts.find(_.customerId == recipientCustomer.id).getOrElse(Account(0, 0, 0.0)), transferAmount) match {
////      case Success((senderAccount, recipientAccount)) =>
////        updateAccountInformation(senderAccount)
////        updateAccountInformation(recipientAccount)
////
////        val senderTransaction = createTransferTransaction(senderAccount, transferAmount, recipientCustomer.name)
////        val recipientTransaction = createTransferTransaction(recipientAccount, transferAmount, senderAccount.customerId.toString)
////
////        transactions = transactions :+ senderTransaction :+ recipientTransaction
////        updateTransactionsFile()
////
////        println("Transfer successful.")
////        println("Exiting.")
////        System.exit(0)
////
////      case Failure(exception) =>
////        println(s"Error: ${exception.getMessage}")
////    }
////  }
////
////  private def updateAccountInformation(updatedAccount: Account): Unit = {
////    accounts = accounts.map(a => if (a.customerId == updatedAccount.customerId) updatedAccount else a)
////    updateAccountsFile()
////    println(s"Transaction successful. Updated Account Information:\n${bankingService.getAccountInfo(updatedAccount)}")
////  }
////
////  private def createTransferTransaction(account: Account, amount: Double, otherParty: String): Transaction = {
////    Transaction(transactions.length + 1, account.id, s"Transfer (${if (otherParty == account.customerId.toString) "Sent" else "Received"})", amount, otherParty)
////  }
////
////  private def updateTransactionsFile(): Unit = {
////    FileService.writeTransactions("src/main/scala/transactions.csv", transactions)
////  }
////
////  private def updateAccountsFile(): Unit = {
////    FileService.writeAccounts("src/main/scala/accounts.csv", accounts)
////  }
////  private def createAccountChoice(): Unit = {
////    println("Enter your full name:")
////    val name = StdIn.readLine()
////    println("Enter your desired username:")
////    val newUsername = StdIn.readLine()
////    println("Enter your desired password:")
////    val newPassword = StdIn.readLine()
////    println("Enter your current address:")
////    val address = StdIn.readLine()
////
////    customers = AuthService.createAccount(name, newUsername, newPassword, address, customers)
////
////    // Optionally, update the CSV file with the new customer data
////    FileService.writeCustomers("src/main/scala/customers.csv", customers)
////
////    println("Account created successfully. You can now log in.")
////  }
////
////
////  // Ask the user if they want to log in or create a new account
////  println("Choose an option:")
////  println("1. Log in")
////  println("2. Create a new account")
////  print("Enter the number of your choice: ")
////
////  private val optionChoice = StdIn.readInt()
////
////  optionChoice match {
////    case 1 =>
////      // User chose to log in
////      println("Enter your username:")
////      val username = StdIn.readLine()
////      println("Enter your password:")
////      val password = StdIn.readLine()
////
////      AuthService.login(username, password, customers) match {
////        case Some(customer) => handleLoggedInCustomer(customer)
////        case None => println("Login failed. Invalid username or password.")
////      }
////
////    case 2 =>
////      // User chose to create a new account
////      createAccountChoice()
////
////    case _ =>
////      println("Invalid choice. Exiting.")
////  }
////
////  // Shutdown the Actor System
////  actorSystem.terminate()
////
////}
/////////////////////////////////////////////////////////

