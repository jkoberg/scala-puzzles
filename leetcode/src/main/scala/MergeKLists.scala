object MergeKLists {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = {
    var outputList: ListNode = null
    var tail: ListNode = null

    val pq = collection.mutable.PriorityQueue.empty[ListNode](Ordering.by[ListNode, Int](_.x).reverse)

    pq.addAll(lists.filter(_ != null))

    while (pq.nonEmpty) {
      val minNode = pq.dequeue()
      if (outputList == null) {
        outputList = minNode
        tail = minNode
      } else {
        tail.next = minNode
        tail = minNode
      }

      if (minNode.next != null) {
        pq.addOne(minNode.next)
      }
    }
    outputList
  }

}
