object ContiguousSubarrays {


  def leftSubArrays(arr:Array[Int]): Array[Int] = {
    val stack = collection.mutable.Stack.empty[Int]
    val L: Array[Int] = Array.fill(arr.length)(0)
    var lasti = 0
    for {
      (n, i) <- arr.tail.zipWithIndex
    } {
      if (n < arr(lasti)) {
        stack.push(lasti)
      } else {
        while (stack.nonEmpty && arr(stack.head) < n) {
          stack.pop()
        }
        val subarrayStart =
          if (stack.isEmpty) {
            0
          } else {
            stack.head + 1
          }
        val numSubarrays = i - subarrayStart
        L(i) = numSubarrays
      }
      lasti = i
    }
    L
  }

  def countSubarrays(arr: Array[Int]): Int = {
    val L = leftSubArrays(arr)
    val R = leftSubArrays(arr.reverse)
    var max = 1
    for {
      i <- arr.indices
    } {
      val subsAtI = 1 + (L(i) * R(i))
      max = subsAtI.max(max)
    }
    max
  }
}
