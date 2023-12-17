// Account.scala
case class Account(id: Int, customerId: Int, balance: Double, transactions: List[Transaction] = List())
