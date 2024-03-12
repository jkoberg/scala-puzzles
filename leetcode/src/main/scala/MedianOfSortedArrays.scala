

object MedianOfSortedArrays {

  def isEven(n:Int) = math.floorMod(n, 2) == 0

  def average(n0:Int, n1: Int): Double = (n0.toLong + n1.toLong) / 2.0

  def arrMedian(nums:Array[Int]): Double = {
    val center = math.floorDiv(nums.length, 2)
    if(isEven(nums.length)) {
      (nums(center).toLong + nums(center - 1).toLong) / 2.0
    } else {
      nums(center)
    }
  }

  def medianForNonOverlapping(smaller:Array[Int], larger:Array[Int]): Double = {
    var totalElems = smaller.length + larger.length
    var center = totalElems / 2
    (isEven(totalElems), smaller.length, larger.length) match {
      // even, center, center -1 are both in smaller array
      case (true, sl, ll) if center < sl => average(smaller(center - 1), smaller(center))
      // even, center is last element of smaller, first element of larger
      case (true, sl, ll) if center == sl => average(smaller(center - 1), larger(0))
      // even, center, center -1 are both in larger array
      case (true, sl, ll) => average(larger(center - sl), larger(center - sl - 1))
      // odd, center is in smaller array,
      case (false, sl, ll) if center < sl => smaller(center)
      // odd, center is in larger array
      case (false, sl, ll) => larger(center - sl)
    }
  }

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
    (nums1.length, nums2.length) match {
      case (0, _) => arrMedian(nums2)
      case (_, 0) => arrMedian(nums1)
      case (s1, s2) if nums2.head >= nums1.last => medianForNonOverlapping(nums1, nums2)
      case (s1, s2) if nums1.head >= nums2.last => medianForNonOverlapping(nums2, nums1)
      case (s1, s2) =>
        var totalElems = s1 + s2
        var leftSideItems = totalElems / 2
        var nums1elems = s1 / 2
        var nums2elems = leftSideItems - nums1elems
        ???
    }
  }


}
