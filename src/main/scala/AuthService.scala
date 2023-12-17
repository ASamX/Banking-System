
object AuthService {
  def login(username: String, password: String, customers: List[Customer]): Option[Customer] = {
    customers.find(customer => customer.username == username && customer.password == password)
  }

  def createAccount(name: String, username: String, password: String, address: String, customers: List[Customer]): List[Customer] = {
    val newCustomerId = customers.length + 1
    val newCustomer = Customer(newCustomerId, username, password, name, address, "")

    // Append the new customer to the 'customers' list
    val updatedCustomers = newCustomer :: customers

    // Write the updated customers back to the CSV file
    FileService.writeCustomers("src/main/scala/customers.csv", updatedCustomers)

    // Retrieve the actual customer with the assigned ID
    val actualNewCustomer = updatedCustomers.find(_.username == username).getOrElse(newCustomer)

    // Create the associated account with the correct customer ID and a new account ID
    val newAccountId = FileService.readAccounts("src/main/scala/accounts.csv").length + 1
    val newAccount = Account(newAccountId, actualNewCustomer.id, 0.0)

    // Read the existing accounts from the CSV file
    val existingAccounts = FileService.readAccounts("src/main/scala/accounts.csv")

    // Append the new account to the 'accounts' list
    val updatedAccounts = newAccount :: existingAccounts

    // Write the updated accounts back to the CSV file
    FileService.writeAccounts("src/main/scala/accounts.csv", updatedAccounts)

    // Return the updated customers list
    updatedCustomers
  }
}
