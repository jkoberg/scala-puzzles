object FourSum {

  def twosum(nums: Array[Int], target: Long, firstIndex: Int = 0): List[List[Int]] = {
    var first = firstIndex
    var last = nums.length - 1
    var output = List.empty[List[Int]]
    while(first < last) {
      val firstn = nums(first)
      val lastn = nums(last)
      if (first != firstIndex && firstn == nums(first - 1)) {
        first = first + 1
      } else if (last != (nums.length - 1) && lastn == nums(last + 1)) {
        last = last - 1
      } else {
        val total = firstn + lastn
        total match {
          case t if t == target =>
            output = List(firstn, lastn) +: output
            first = first + 1
            last = last - 1
          case t if t < target =>
            first = first + 1
          case t if t > target =>
            last = last -1
        }
      }
    }
    output
  }

  def ksum_rec(k: Int, nums: Array[Int], target: Long, firstIndex:Int = 0): List[List[Int]] = {
    if(k == 2) {
      twosum(nums, target, firstIndex)
    } else {
      var output = List.empty[List[Int]]
      for {
        i <- firstIndex.until(nums.length - (k - 1))
        n = nums(i)
        if i == firstIndex || n != nums(i-1)
        matchlist <- ksum_rec(k - 1, nums, target-n, i+1)
      }  {
        output = (n +: matchlist) +: output
      }
      output
    }
  }

  def ksum(k:Int, nums: Array[Int], target:Long =0) = {
    util.Sorting.quickSort(nums)
    ksum_rec(k, nums, target)
  }



  def main(args: Array[String]): Unit = {
    for {
      m <- ksum(3, Array(-1, -1, 0, 0, 1, 1, 2, -2, 2, -2, 0))
    } do {
      println(s"$m")
    }

  }
}
