
object ArrayPartition {

  def validPartition2(nums: Array[Int]): Boolean = {
    val validPartitionEndingAt = collection.mutable.BitSet.empty

    inline def validLen2SubarrayEndsAt(i: Int): Boolean =
      i >= 1 && (
        nums(i) == nums(i - 1)
      )

    inline def validLen3SubarrayEndsAt(i: Int): Boolean =
      i >= 2 && {
        (nums(i) == nums(i-1) && nums(i) == nums(i-2))
          || (nums(i) - 1 == nums(i - 1) && nums(i) - 2 == nums(i - 2))
      }

    var i = 0
    var exploreTo = 2
    while(i < nums.length && i <= exploreTo) {
      if (
        (validLen2SubarrayEndsAt(i) && (i == 1 || validPartitionEndingAt(i - 2)))
        || (validLen3SubarrayEndsAt(i) && (i == 2 || validPartitionEndingAt(i - 3)))
      ) {
        validPartitionEndingAt(i) = true
        exploreTo = exploreTo.max(i + 3)
      }
      i = i + 1
    }
    validPartitionEndingAt(nums.length - 1)
  }

  def validPartition(nums: Array[Int]): Boolean = {
    val heads = collection.mutable.Stack[Int](0)
    val invalids = collection.mutable.HashSet.empty[Int]

    val length = nums.length

    def len3match(i: Int): Boolean = {
      i <= length - 3 && (
        (nums(i) == nums(i + 1) && nums(i) == nums(i + 2))
          ||
          (nums(i) + 1 == nums(i + 1) && nums(i) + 2 == nums(i + 2))
        )
    }

    def len2match(i: Int): Boolean = {
      i <= length - 2 &&
        (nums(i) == nums(i + 1))
    }


    while(heads.nonEmpty) {
      val current = heads.head
      if(current == nums.length) {
        return true
      } else {
        var pushed = false
        if(len2match(current) && !invalids.contains(current + 2)) {
          heads.push(current + 2)
          pushed = true
        }
        if(len3match(current) && !invalids.contains(current + 3)) {
          heads.push(current + 3)
          pushed = true
        }
        if(!pushed) {
          invalids.add(current)
          heads.pop()
        }
      }
    }
    false
  }
}
