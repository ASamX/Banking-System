// Transaction.scala
//case class Transaction(id: Int, accountId: Int, transactionType: String, amount: Double)
// Transaction.scala
case class Transaction(id: Int, accountId: Int, transactionType: String, amount: Double, otherParty: String = "")
