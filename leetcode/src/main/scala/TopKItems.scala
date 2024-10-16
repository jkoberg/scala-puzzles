class TopKItems {

  /**
   * Find k most frequent items
   * Traverse input and count occurrences into hashmap
   * Keep min-queue of size K, containing (count, item) pairs
   * For each item, if the count is greater than the minimum item on the heap,
   * dequeue the minimum item and insert the current item
   * 
   * Time: O(n log k)
   * Space: O(n + k)
   * 
   * @param items
   * @param k
   * @return
   */
  def topKItems(items: Seq[Int], k: Int): Seq[Int] = {
    val counts = collection.mutable.HashMap.empty[Int, Int]
    val topK = collection.mutable.PriorityQueue.empty[(Int, Int)](Ordering.by((x:(Int, Int)) => x._1).reverse)
    for {
      item <- items
    } {
      val currentCount = counts.updateWith(item) {
        case Some(value) => Some(value + 1)
        case None => Some(1)
      }.get
      if(topK.size < k) {
        topK.addOne((currentCount, item))
      } else {
        val (lowCount, lowItem) = topK.head
        if(lowCount < currentCount) {
          topK.dequeue()
          topK.addOne((currentCount, item))
        }
      }
    }
    topK.map(_._2).toSeq
  }
}