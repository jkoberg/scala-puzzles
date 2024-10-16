
object NumberOfOrdersInBacklog {

  def getNumberOfBacklogOrders(orders: Array[Array[Int]]): Int = {
    type BookItem = (Int, Long)
    type Book = collection.mutable.PriorityQueue[BookItem]

    val bids: Book = collection.mutable.PriorityQueue.empty[BookItem]
    val asks: Book = collection.mutable.PriorityQueue.empty[BookItem](Ordering.Tuple2[Int, Long].reverse)

    var backloggedCount = 0L

    def enqueue(q: Book, item: BookItem): Unit = {
      q.headOption match
        case Some((price, qty)) if price == item._1 =>
          val oldItem = dequeue(q)
          enqueue(q, (price, oldItem._2 + qty))          
        case _ =>
          q.enqueue(item)
      backloggedCount = backloggedCount + item._2
    }

    def dequeue(q: Book): BookItem = {
      val item = q.dequeue()
      backloggedCount = backloggedCount - item._2
      item
    }

    def execute(amount: Int, price: Int, from: Book, to: Book, compare: (Int, Int) => Boolean): Unit = {
      var remaining = amount.toLong
      while (remaining > 0 && from.nonEmpty && compare(from.head._1, price)) {
        val (matchedPrice, matchedQty) = dequeue(from)
        remaining = remaining - matchedQty
        if(remaining < 0) {
          enqueue(from, (matchedPrice, -remaining))
        }
      }
      if (remaining > 0) {
        enqueue(to, (price, remaining))
      }
    }

    for {
      case Array(price, amount, orderType) <- orders
    } {
      orderType match {
        case 0 => execute(amount, price, asks, bids, _ <= _)
        case 1 => execute(amount, price, bids, asks, _ >= _)
      }
    }

    (backloggedCount % 1_000_000_007).toInt
  }

  def test(): Unit = {
    val input = Array(
      Array(10, 5, 0),
      Array(15, 2, 1),
      Array(25, 1, 1),
      Array(30, 4, 0)
    )
    assert(getNumberOfBacklogOrders(input) == 6)
  }
}