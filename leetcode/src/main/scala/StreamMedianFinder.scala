class StreamMedianFinder() {

  var lower = collection.mutable.PriorityQueue.empty[Int]
  var upper = collection.mutable.PriorityQueue.empty[Int](Ordering.Int.reverse)


  def addNum(num: Int): Unit = {
    if (lower.isEmpty && upper.isEmpty) {
      lower.addOne(num)
    }
    else if (lower.isEmpty && num <= upper.head) {
      lower.addOne(num)
    }
    else if (upper.isEmpty && num >= lower.head) {
      upper.addOne(num)
    }
    else if (num <= lower.head) {
      lower.addOne(num)
    }
    else if (num >= upper.head) {
      upper.addOne(num)
    } else {
      lower.addOne(num)
    }

    if (lower.size > upper.size + 1) {
      upper.addOne(lower.dequeue())
    }
    else if (upper.size > lower.size + 1) {
      lower.addOne(upper.dequeue())
    }
  }

  def findMedian(): Double = {
    if (lower.size == upper.size) {
      (lower.head + upper.head) / 2.0
    }
    else if (lower.size > upper.size) {
      lower.head
    }
    else upper.head
  }
}
