object SimpleBank {

  class Bank(_balance: Array[Long]) {
    private def validateAccountNumber(account: Int): Option[Int] = {
      val baseAdjusted = account - 1
      if(baseAdjusted >= _balance.length) {
        None
      } else {
        Some(baseAdjusted)
      }
    }

    def transfer(account1: Int, account2: Int, money: Long): Boolean = {
      (for {
        from <- validateAccountNumber(account1)
        to <- validateAccountNumber(account2)
        if _balance(from) >= money
      } yield {
        _balance(from) = _balance(from) - money
        _balance(to) = _balance(to) + money
        true
      }).getOrElse(false)
    }

    def deposit(account: Int, money: Long): Boolean = {
      validateAccountNumber(account).exists { to =>
        _balance(to) = _balance(to) + money
        true
      }
    }

    def withdraw(account: Int, money: Long): Boolean = {
      (for {
        from <- validateAccountNumber(account)
        if _balance(from) >= money
      } yield {
        _balance(from) = _balance(from) - money
        true
      }).getOrElse(false)
    }

  }


}
